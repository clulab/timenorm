package org.clulab.timenorm.scate

import java.io._
import java.nio.file._

import org.apache.commons.io.IOUtils
import org.clulab.anafora.Data
import org.tensorflow.{Graph, Session, Tensor}
import play.api.libs.json._

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.io.Source
import scala.language.postfixOps
import scala.xml.{XML, Elem}


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
      val xml = parser.parseToXML(text)
      Files.createDirectories(outPath.getParent)
      XML.save(outPath.toString, xml)
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
      val outPath = outPathNoExt.resolveSibling(outPathNoExt.getFileName.toString + outExt)
      (inPath, outPath)
    }
  }
}


class TemporalNeuralParser(modelStream: Option[InputStream] = None) extends AutoCloseable {

  private def resourceLines(path: String): Iterator[String] = {
    Source.fromInputStream(this.getClass.getResourceAsStream(path)).getLines()
  }

  // the neural network that reads characters and predicts operators
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

  // map from indices to operator names
  lazy private val operatorLabels: IndexedSeq[String] = {
    resourceLines("/org/clulab/timenorm/label/operator.txt").toIndexedSeq
  }

  // map from indices to non-operator names
  lazy private val nonOperatorLabels: IndexedSeq[String] = {
    resourceLines("/org/clulab/timenorm/label/non-operator.txt").toIndexedSeq
  }

  // lookup table from operator name to text expression to normalized value for operator's type
  lazy private val operatorToTextToType: Map[String, Map[String, String]] = {
    resourceLines("/org/clulab/timenorm/linking_configure/date-types.txt").map(_.split(' ')).map{
      case Array(operator, property, string) => (operator, (string, property))
    }.toIndexedSeq.groupBy(_._1).mapValues(_.map(_._2).toMap).toMap
  }

  // textual indicators that help to filter links for Between operators
  lazy private val betweenIndicators: Map[String, IndexedSeq[String]] = {
    resourceLines("/org/clulab/timenorm/linking_configure/between-indicators.txt").map(_.split(' ')).map{
      case Array(key, string) => (key, string)
    }.toIndexedSeq.groupBy(_._1).mapValues(_.map(_._2))
  }

  lazy private val operatorToPropertyToTypes: Map[String, Map[String, Set[String]]] = {
    val path = "/org/clulab/timenorm/linking_configure/timenorm-schema.xml"
    val xml = XML.load(this.getClass.getResourceAsStream(path))
    val entityInfos = for (entity <- xml \\ "entity") yield {
      // sort so that required properties are first
      val properties = (entity \\ "property").sortBy(_.attributes.get("required").map(_.text).getOrElse("True") == "False")
      // match each operator type with an insertion-ordered map of its property names and their allowable types
      val Some(entityType) = entity.attributes.get("type").map(_.text)
      (entityType, ListMap.empty ++ properties.map{ p =>
        val Some(propertyType) = p.attributes.get("type").map(_.text)
        val instanceOfOption = p.attributes.get("instanceOf").map(_.text)
        (propertyType, instanceOfOption.map(_.split(",").toSet).getOrElse(Set.empty))
      })
    }
    entityInfos.toMap
  }

  // TODO: should be a constructor parameter
  lazy private val textToNumber: WordsToNumber = WordsToNumber("en")

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
    val antixmlCleanedText = """[^\u0009\u000A\u000D\u0020-\uD7FF\uE000-\uFFFD]""".r.replaceAllIn(text, " ")

    val allTimeSpans = identifyBatch(text, spans)
    val timeSpanToId = allTimeSpans.flatten.zipWithIndex.toMap.mapValues(i => s"$i@id")
    for (timeSpans <- allTimeSpans) yield {
      val entityElems = for ((timeSpan, links) <- timeSpans zip inferLinks(text, timeSpans)) yield {
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
      </data>
    }
  }


  private def identifyBatch(text: String, spans: Array[(Int, Int)]): Array[Array[(Int, Int, String)]] = {
    val wordCharIndices = """\w""".r.findAllMatchIn(text).map(_.start).toSet
    val snippetStarts = spans.map { case (start, end) => (text.substring(start, end), start) }
    val expandedTexts = spans.map { case (start, end) => "\n\n\n" + text.substring(start, end) + "\n\n\n" }
    val maxLen = expandedTexts.map(_.length).max
    val inputArray = expandedTexts.map(_.map(char2Index).padTo(maxLen, 4f).toArray)
    val input = Tensor.create(inputArray)

    // Run forward pass
    val tensors = this.network.runner()

      .feed("character",input)
      .fetch("dense_1/truediv").fetch("dense_2/truediv").fetch("dense_3/truediv")
      .run().asScala

    val Seq(nonOperators, expOperators, impOperators) = for {
      (labelVocabulary, tensor) <- Seq(nonOperatorLabels, operatorLabels, operatorLabels) zip tensors
    } yield {
      val Array(nBatches, nChars, nCategories) = tensor.shape.map(_.toInt)
      val allPredictions = tensor.copyTo(Array.ofDim[Float](nBatches, nChars, nCategories))
      for (((snippet, snippetStart), predictions) <- snippetStarts zip allPredictions) yield {

        // select the most probable label for each character
        val predictedLabels = for (categoryScores <- predictions.slice(3, 3 + snippet.length)) yield {
          val maxScore = categoryScores.max
          val maxIndex = categoryScores.indexWhere(_ == maxScore)
          labelVocabulary.lift(maxIndex - 1).getOrElse("O")
        }

        // Slide through label list and get position where the label type changes.
        val changesWithIndex = for {
          (Array(label1, label2), index) <- ("O" +: predictedLabels :+ "O").sliding(2).zipWithIndex
          if label1 != label2
        } yield (label2, snippetStart + index)

        // build the spans as current_position(start), next_position(end), current_label. Remove the "O"s
        val labeledSpans = for {
          Seq((label, start), (_, end)) <- changesWithIndex.sliding(2)
          if label != "O"
        } yield {

          // Complete the annotation if the span does not cover the whole token (but only expand over "O" labels)
          val doExpand = (i: Int) => wordCharIndices.contains(i) && predictedLabels.lift(i - snippetStart).contains("O")
          val wordStart = Iterator.from(start - 1, -1).takeWhile(doExpand).toSeq.lastOption.getOrElse(start)
          val wordEnd = Iterator.from(end, +1).takeWhile(doExpand).toSeq.lastOption.map(_ + 1).getOrElse(end)
          (wordStart, wordEnd, label)
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

  def filterBetween(property: String, text: String, source: (Int, Int, String), target: (Int, Int, String)): Boolean = {
    val source_text = text.slice(source._1, source._2)
    property match {
      case "End-Interval" if source._1 > target._1 || betweenIndicators.getOrElse("Start", IndexedSeq()).contains(source_text) => false
      case "Start-Interval" if source._1 < target._1 && betweenIndicators.getOrElse("End", IndexedSeq()).contains(source_text) => false
      case _ => true
    }
  }

  def inferLinks(text: String, timeSpans: Array[(Int, Int, String)]): Array[Array[(String, Int)]] = {
    val links = Array.fill(timeSpans.length)(mutable.ArrayBuffer.empty[(String, Int)])
    val ancestors = Array.fill(timeSpans.length)(mutable.Set.empty[Int])
    val descendants = Array.fill(timeSpans.length)(mutable.Set.empty[Int])
    var start = 0
    for ((timeSpan, i) <- timeSpans.zipWithIndex) {

      // if we're more than 10 characters away, discard previous history
      if (start != i && timeSpan._1 - timeSpans(i-1)._2 > 10)
        start = i

      // find relations between the current and any previous time spans
      for (s <- (start until i).reverse) {

        // find relations that are valid
        for {
          (source, target) <- Seq((s, i), (i, s))
          sourceType = timeSpans(source)._3
          targetType = timeSpans(target)._3
          propertyAllowedValues <- this.operatorToPropertyToTypes.get(sourceType)
          (propertyName, allowedValues) <- propertyAllowedValues.filterKeys(filterBetween(_, text, timeSpans(source), timeSpans(target)))
          // the slot should be valid according to the schema
          if allowedValues contains targetType
          // the slot should not already be full
          if !links(source).map(_._1).contains(propertyName)
          // the target should not already be filling someone else's slot
          if ancestors(target).isEmpty
          // don't create cyclic links
          if !ancestors(source).contains(target) && !descendants(target).contains(source)
        } {
          links(source) += propertyName -> target

          // update data structures for avoiding cyclic links
          ancestors(target) += source
          ancestors(target) ++= ancestors(source)
          descendants(source) += target
          descendants(source) ++= descendants(target)
        }
      }
    }
    links.map(_.toArray)
  }

  private def inferProperties(timeText: String, timeType: String, links: Array[(String, Int)]): Array[(String, String)] = {
    val propertyOptions = for (propertyType <- this.operatorToPropertyToTypes(timeType).keys) yield propertyType match {
       case "Type" =>
         // The "Type" value for decades (70s, 80s, ...) is set as "Years".
         // It will be converted into "Year" in case of "Period". E.g. "He is in his 70s".
         val timeTextNoDecades = """"^[0-9]{2}s?$""".r.replaceAllIn(timeText, "Years")
         val p = this.operatorToTextToType.get(timeType).flatMap(_.get(timeTextNoDecades.toLowerCase)).getOrElse("Unknown")
         (timeType, p.last) match {
           case ("Calendar-Interval", 's') => Some((propertyType, p.dropRight(1)))
           case ("Period", l) if p != "Unknown" && l != 's' => Some((propertyType, p + "s"))
           case ("Frequency", _) => Some((propertyType, "Other"))
           case _ => Some((propertyType, p))
         }
       case "Value" =>
         val cleanedText = """^0*(\d+)[^\d]*$""".r.replaceAllIn(timeText, _.group(1))
         val valueOption =
           try {
             Some(cleanedText.toLong)
           } catch {
             case _: NumberFormatException => textToNumber(cleanedText.split("""[\s-]+"""))
           }
         Some((propertyType, valueOption.map(_.toString).getOrElse(timeText)))
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

  override def close(): Unit = network.close()
}
