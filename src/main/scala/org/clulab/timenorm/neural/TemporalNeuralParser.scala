package org.clulab.timenorm.neural

import java.io._
import java.nio.file.{FileSystems, Files, Path, PathMatcher, Paths}

import scala.collection.JavaConverters._
import com.codecommit.antixml._
import org.clulab.anafora.Data
import org.clulab.timenorm.formal._
import org.tensorflow.Graph
import org.tensorflow.Session
import org.tensorflow.Tensor
import org.apache.commons.io.IOUtils
import play.api.libs.json._

import scala.collection.mutable
import scala.io.Source
import scala.language.postfixOps
import scala.util.Try


object TemporalNeuralParser {
  val timeNormExt = ".TimeNorm.system.completed.xml"
  val usage =
    s"""
       |usage: ${this.getClass.getSimpleName} INPUT [OUTPUT]
       |
       |Parses the input text file(s) for time expressions and writes them out as
       |Anafora XML. Output files will match the name (and subdirectory structure)
       |of the input files, adding an $timeNormExt extension.
       |
       |arguments:
       |  INPUT    Either a single text file, or a directory. INPUT should be text;
       |           XML files will be silenty ignored.
       |  OUTPUT   Either a single output file prefix (if INPUT was a single file) or
       |           a directory (if INPUT was a directory).
     """.stripMargin

  def main(args: Array[String]): Unit = args match {
    case Array(inputDir, outputDir) =>
      parseAll(Paths.get(inputDir), Paths.get(outputDir))
    case Array(dir) =>
      val path = Paths.get(dir)
      parseAll(path, path)
    case _ =>
      println(usage)
      sys.exit(1)
  }

  def parseAll(inRoot: Path, outRoot: Path): Unit = {
    val parser = new TemporalNeuralParser()
    val endsWithXML = FileSystems.getDefault.getPathMatcher("glob:**.xml")
    for ((inPath, outPath) <- parallelPaths(inRoot, outRoot, endsWithXML, timeNormExt)) {
      val text = new String(Files.readAllBytes(inPath))
      val xml: Elem = parser.parseToXML(text)
      Files.createDirectories(outPath.getParent)
      val writer = Files.newBufferedWriter(outPath)
      xml.writeTo(writer)
      writer.close()
      println(s"$inPath\n$outPath\n")
    }
  }

  def parallelPaths(inRoot: Path, outRoot: Path, inExclude: PathMatcher, outExt: String): Iterator[(Path, Path)] = {
    for {
      inPath <- Files.walk(inRoot).iterator.asScala
      if Files.isRegularFile(inPath) && !inExclude.matches(inPath)
    } yield {
      val subPath = inRoot.relativize(inPath)
      val outPathNoExt = outRoot.resolve(subPath)
      val outPath = outPathNoExt.resolveSibling(outPathNoExt.getFileName + outExt)
      (inPath, outPath)
    }
  }
}


class TemporalNeuralParser(modelStream: Option[InputStream] = None) {

  lazy private val network = {
    val graph = new Graph()
    graph.importGraphDef(IOUtils.toByteArray(
      modelStream.getOrElse(this.getClass.getResourceAsStream("/org/clulab/timenorm/model/weights-improvement-22.pb"))))
    new Session(graph)
  }

  // indices are Ints, but since tensors are filled with Float, it's more efficient to save them as Float
  lazy private val char2Index: Map[Char, Float] = {
    val dictStream = this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/dictionary.json")
    try {
      val string2int = Json.parse(dictStream).as[Map[String, Int]]
      val char2int = for ((string, value) <- string2int; if string.length == 1) yield (string.head, value.toFloat)
      char2int.withDefaultValue(string2int("<unk>"))
    } finally {
      dictStream.close()
    }
  }

  lazy private val operatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/operator.txt")).getLines.toIndexedSeq
  lazy private val nonOperatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/non-operator.txt")).getLines.toIndexedSeq
  lazy private val types = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/linking_configure/date-types.txt")).getLines
                            .map(_.split(' ')).map(a => (a(0), (a(2), a(1)))).toIndexedSeq.groupBy(_._1).mapValues(_.map(_._2).toMap)

  // TODO: this is ugly
  lazy private val schema = (for {
    es <- scala.xml.XML.load(this.getClass.getResourceAsStream("/org/clulab/timenorm/linking_configure/timenorm-schema.xml")) \\ "entities"
    e <- es \ "entity"
  } yield (
    (e \\ "property").map(p =>
      ((e \ "@type" head).toString, ((p \ "@type" head).toString, (Try((p \ "@required" head).toString.toBoolean).getOrElse(true), Try((p \ "@instanceOf" head).toString.split(",")).getOrElse(Array()))))
    )
      :+
      ((e \ "@type" head).toString, ("parentType", (true, Array((es \ "@type" head).toString))))
    )).flatten.groupBy(_._1).mapValues(_.map(_._2).toMap)

  def parse(text: String, textCreationTime: Interval = UnknownInterval()): Array[TimeExpression] = {
    parseBatch(text, Array((0, text.length)), textCreationTime) match {
      case Array(timeExpressions) => timeExpressions
    }
  }

  def parseToXML(text: String): Elem = {
    parseBatchToXML(text, Array((0, text.length))) match {
      case Array(xml) => xml
    }
  }

  def parseBatch(text: String,
                 spans: Array[(Int, Int)],
                 textCreationTime: Interval = UnknownInterval()): Array[Array[TimeExpression]] = {
    for (xml <- parseBatchToXML(text, spans)) yield {
      implicit val data: Data = new Data(xml, Some(text))
      val reader = new AnaforaReader(textCreationTime)
      data.topEntities.map(reader.temporal).toArray
    }
  }

  def parseBatchToXML(text: String, spans: Array[(Int, Int)]): Array[Elem] = {
    val antixmlCleanedText = """[^\u0009\u000A\u000D\u0020-\uD7FF\uE000-\uFFFD]+""".r.replaceAllIn(text, " ")

    val allTimeSpans = identifyBatch(text, spans)
    val timeSpanToId = allTimeSpans.flatten.zipWithIndex.toMap.mapValues(i => s"$i@id")
    for (timeSpans <- allTimeSpans) yield {
      val entityElems = for ((timeSpan, links) <- timeSpans zip inferLinks(timeSpans)) yield {
        val id = timeSpanToId(timeSpan)
        val (start, end, timeType) = timeSpan
        val timeText = antixmlCleanedText.substring(start, end)
        val linkProperties = for ((relationType, childIndex) <- links) yield
          <xml>{timeSpanToId(timeSpans(childIndex))}</xml>.copy(label = relationType)
        val textProperties = for ((propertyType, propertyValue) <- inferProperties(timeText, timeType, links)) yield
          <xml>{propertyValue}</xml>.copy(label = propertyType)
        <entity>
          <id>{id}</id>
          <span>{s"$start,$end"}</span>
          <type>{timeType}</type>
          <properties>
            {linkProperties ++ textProperties}
          </properties>
        </entity>
      }
      <data>
        <annotations>
          {entityElems}
        </annotations>
      </data>.convert
    }
  }


  private def identifyBatch(text: String, spans: Array[(Int, Int)]): Array[Array[(Int, Int, String)]] = {
    val expandedTexts = spans.map {
      case (start, end) => "\n\n\n" + text.substring(start, end) + "\n\n\n"
    }
    val maxLen = expandedTexts.map(_.length).max
    val inputArray = expandedTexts.map(_.map(char2Index).padTo(maxLen, 4f).toArray)
    val input = Tensor.create(inputArray)

    val re = """[^a-zA-Z\d]""".r
    val textInfos = spans.map {
      case (start, end) =>
        val snippet = text.substring(start, end)
        (start, snippet, re.findAllMatchIn(snippet).map(start + _.start).toSeq)
    }

    // Run forward pass
    val tensors = this.network.runner()

      .feed("character",input)
      .fetch("dense_1/truediv").fetch("dense_2/truediv").fetch("dense_3/truediv")
      .run().asScala

    val Seq(nonOperators, expOperators, impOperators) = for {
      (labels, tensor) <- Seq(nonOperatorLabels, operatorLabels, operatorLabels) zip tensors
    } yield {
      val Array(nBatches, nChars, nCategories) = tensor.shape.map(_.toInt)
      val allPredictions = tensor.copyTo(Array.ofDim[Float](nBatches, nChars, nCategories))
      for (((textStart, snippet, spaceIndices), predictions) <- textInfos zip allPredictions) yield {

        // select the most probable label for each character
        val predictedLabels = for (categoryScores <- predictions.slice(3, 3 + snippet.length)) yield {
          val maxScore = categoryScores.max
          val maxIndex = categoryScores.indexWhere(_ == maxScore)
          labels.lift(maxIndex - 1).getOrElse("O")
        }

        // Slide through label list and get position where the label type changes.
        val changesWithIndex = for {
          (Array(label1, label2), index) <- ("" +: predictedLabels :+ "").sliding(2).zipWithIndex
          if label1 != label2
        } yield (label2, textStart + index)

        // build the spans as current_position(start), next_position(end), current_label. Remove the "O"s
        val labeledSpans = for {
          Seq((label, start), (_, end)) <- changesWithIndex.sliding(2)
          if label != "O"
        } yield {

          // Complete the annotation if the span does not cover the whole token
          val startAdjustment = Try(spaceIndices.map((start - 1) - _).filter(_ >= 0).min).getOrElse(0)
          val endAdjustment = Try(spaceIndices.map(_ - end).filter(_ >= 0).min).getOrElse(0)
          (start - startAdjustment, end + endAdjustment, label)
        }

        labeledSpans.toArray
      }
    }

    // for each batch, combine the different time operators into a single array
    for (i <- expandedTexts.indices.toArray) yield {
      val operators = nonOperators(i) ++ expOperators(i) ++ impOperators(i)
      operators.sortBy { case (start, _, _) => start }
    }
  }

  private def inferLinks(timeSpans: Array[(Int, Int, String)]): Array[Array[(String, Int)]] = {
    val links = Array.fill(timeSpans.length)(mutable.ArrayBuffer.empty[(String, Int)])
    val stack = Array(0, 0)
    for ((timeSpan, i) <- timeSpans.zipWithIndex) {
      if (stack(0) != stack(1) && timeSpan._1 - timeSpans(stack(1)-1)._2 > 10)
        stack(0) = stack(1)
      for {
        s <- (stack(0) until stack(1)).toList.reverse
        label <- relation(timeSpans(s)._3, timeSpan._3).orElse(relation(timeSpan._3, timeSpans(s)._3))
      } {
        links(s) += label -> i
      }
      stack(1) += 1
    }
    links.map(_.toArray)
  }

  private def inferProperties(timeText: String, timeType: String, links: Array[(String, Int)]): Array[(String, String)] = {
    val propertyOptions = for (propertyType <- this.schema(timeType).keys) yield propertyType match {
       case "Type" =>
         val p = Try(this.types(timeType)(timeText)).getOrElse(timeText).toString
         (timeType, p.last) match {
           case ("Calendar-Interval", 's') => Some((propertyType, p.dropRight(1)))
           case ("Period", l) if p != "Unknown" && l != 's' => Some((propertyType, p + "s"))
           case _ => Some((propertyType, p))
         }
       case "Value" =>
         val rgx = """^0*(\d+)[^\d]*$""".r
         Some((propertyType, WordToNumber.convert(rgx.replaceAllIn(timeText, _.group(1)))))
       case intervalType if intervalType contains "Interval-Type" =>
         if (links.exists{ case (relationType, _) => intervalType contains relationType})
           Some((propertyType, "Link"))
         else
           Some((propertyType, "DocTime"))
       case "Semantics" => timeType match {
         case "Last" => Some((propertyType, "Interval-Not-Included")) // TODO: Include journal Last
         case _ => Some((propertyType, "Interval-Not-Included"))
       }
       case intervalType if intervalType contains "Included" =>
         Some((propertyType, "Included"))
       case _ => None
     }
    propertyOptions.flatten.toArray
  }

  private def relation(type1: String, type2: String): Option[String] = {
    // TODO: this is ugly
    Try(Some(this.schema(type1).toList.filter(_._2._2 contains type2).map(_._1).sorted.reverse.head)).getOrElse(None)
  }
}
