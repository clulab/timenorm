package org.clulab.timenorm.neural

import java.io._
import java.nio.file.{Files, Paths}
import java.time.{LocalDateTime, ZoneOffset}

import com.codecommit.antixml._
import org.clulab.anafora.Data
import org.clulab.timenorm.formal._
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.util.ModelSerializer
import org.nd4j.linalg.factory.Nd4j
import play.api.libs.json._

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

object TemporalNeuralParser {
  val usage =
    """
          Usage: TemporalNeuralParser -i FILE -o FILE [-b NUMBER]

          Arguments:
            -i FILE, --input FILE
            -o FILE, --output FILE
            -b NUMBER, --batch_size NUMBER    Default: 40

            -h, --help                        prints this menu
    """

  def main(args: Array[String]): Unit = {

    def exit(message: Option[String] = None) = {
      if (message.isDefined) println(message.get)
      println(usage)
      sys.exit(1)
    }

    def parseOptions(argList: List[String]): Map[String, String] = {
      if (args.length == 0 || args(0) == "-h" || args(0) == "--help") exit()
      argList.sliding(2, 2).map(s =>
        s.head match {
          case "--input" | "-i" =>
            if (Files.exists(Paths.get(s(1))))
              "input" -> s(1)
            else
              exit(Some("Input file does not exist."))
          case "--output" | "-o" =>
            if (Files.exists(Paths.get(s(1))))
              "output" -> s(1)
            else
              exit(Some("Output file does not exist."))
          case "--batch_size" | "-b" =>
            if (s(1).forall(_.isDigit))
              "batch_size" -> s(1)
            else
              exit(Some("Batch size is not a number."))
          case _ => exit(Some("Bad usage."))
        }
      ).toMap
    }
    val options = parseOptions(args.toList)
    if (!(options.contains("input") && options.contains("output")))
      exit(Some("Bad usage."))

    val parser = new TemporalNeuralParser()
    val file = new File(options("output"))
    val bw = new BufferedWriter(new FileWriter(file))
    val batch_size = options.getOrElse("batch_size", "40").toInt
    val batches = Source.fromFile(options("input")).getLines.toList.sliding(batch_size, batch_size).toList
    for ((batch, batchn) <- batches.zipWithIndex) {
      println("Batch " + (batchn + 1) + "/" + batches.size + " with " + batch.size + " lines")
      val data = parser.parse(batch)
      val lines = parser.intervals(data)
      for ((line, linen) <- lines.zipWithIndex) {
        println("\tLine number " + (batchn * batch_size + linen) + ": " + batch(linen))
        bw.write("Line number: " + (batchn * batch_size + linen) + "\n")
        for ((timex, index) <- line.zipWithIndex) {
          bw.write("\tTimEx " + index + ":" + "\n")
          bw.write("\t\tText: " + batch(linen).slice(timex._1._1, timex._1._2) + "\n")
          bw.write("\t\tSpan: " + timex._1._1 + " - " + timex._1._2 + "\n")
          bw.write("\t\tIntervals:" + "\n")
          for (interval <- timex._2)
            bw.write("\t\t\t" + interval._1 + "-" + interval._2 + " " + interval._3 + "\n")
        }
        bw.write("\n")
      }
    }
    println("Finished!")
    bw.close()
  }
}


class TemporalNeuralParser(modelFile: Option[InputStream] = None) {
  private type Entities = List[List[(Int, Int, String)]]
  private type Properties = List[List[(Int, String, String)]]

  lazy private val network: ComputationGraph = ModelSerializer.restoreComputationGraph(
    modelFile.getOrElse(this.getClass.getResourceAsStream("/org/clulab/timenorm/model/weights-improvement-22.dl4j.zip")), false)
  lazy private val char2int = readDict(this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/dictionary.json"))
  lazy private val operatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/operator.txt")).getLines.toList
  lazy private val nonOperatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/non-operator.txt")).getLines.toList
  lazy private val types = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/linking_configure/date-types.txt")).getLines
                            .map(_.split(' ')).map(a => (a(0), (a(2), a(1)))).toList.groupBy(_._1).mapValues(_.map(_._2).toMap)
  lazy private val schema = (for {
      es <- scala.xml.XML.load(this.getClass.getResourceAsStream("/org/clulab/timenorm/linking_configure/timenorm-schema.xml")) \\ "entities"
      e <- es \ "entity"
    } yield (
      (e \\ "property" ).map(p =>
        ((e \ "@type" head).toString, ((p \ "@type" head).toString, (Try((p \ "@required" head).toString.toBoolean).getOrElse(true), Try((p \ "@instanceOf" head).toString.split(",")).getOrElse(Array()))))
      )
      :+
      ((e \ "@type" head).toString, ("parentType", (true, Array((es \ "@type" head).toString))))
  )).flatten.groupBy(_._1).mapValues(_.map(_._2).toMap)


  def printModel(){
    println(this.network.summary())
  }


  private def readDict(dictFile: InputStream): Map[String, Double] = {
    try {  Json.parse(dictFile).as[Map[String, Double]] } finally { dictFile.close() }
  }


  def parse(sourceText: List[String]): List[Data] = synchronized {
    val entities = identification(sourceText)
    val links = linking(entities)
    val antixml_not_allowed = """[^\u0009\u000A\u000D\u0020-\uD7FF\uE000-\uFFFD]+""".r
    val cleanSourceText = sourceText.map(antixml_not_allowed.replaceAllIn(_, " "))
    val properties = complete(entities, links, cleanSourceText)
    val anafora: List[Elem] = build(entities, links, properties)
    val data = (anafora zip cleanSourceText).map(b => new Data(b._1, Some(b._2)))
    data
  }


  def dct(data: Data): Interval = {
    val reader = new AnaforaReader(UnknownInterval)(data)
    val time = Try(reader.temporal(data.topEntities(0))(data)).getOrElse(null)
    time match {
      case interval: Interval => interval
      case intervals: Intervals => intervals.head
      case _ => UnknownInterval
    }
  }


  def extract_interval(interval: Interval): (LocalDateTime, LocalDateTime, Long) = (
      interval.start,
      interval.end,
      interval.end.toEpochSecond(ZoneOffset.UTC) - interval.start.toEpochSecond(ZoneOffset.UTC)
  )


  def intervals(data_batch: List[Data], dct: Option[Interval] = Some(UnknownInterval)): List[List[((Int,Int), List[(LocalDateTime, LocalDateTime, Long)])]] = synchronized {
    data_batch.map(data => {
      val reader = new AnaforaReader(dct.get)(data)
      data.topEntities.map(e => {
        val span = e.expandedSpan(data)
        val time = Try(reader.temporal(e)(data)).getOrElse(null)
        val timeIntervals: List[(LocalDateTime, LocalDateTime, Long)] = Try(time match {
          case interval: Interval => List(extract_interval(interval))
          case intervals: Intervals => intervals.map(extract_interval).toList
          case rInterval: RepeatingInterval => List((
            null,
            null,
            rInterval.preceding(LocalDateTime.now).next.end.toEpochSecond(ZoneOffset.UTC) - rInterval.preceding(LocalDateTime.now).next.start.toEpochSecond(ZoneOffset.UTC)
          ))
          case period: SimplePeriod => List((
            null,
            null,
            period.number * period.unit.getDuration.getSeconds
          ))
          case _ => List((null, null, 0.toLong))
        }).getOrElse(List((null, null, 0.toLong)))
        (span, timeIntervals)
      }).toList
    })
  }


  private def identification(sourceText: List[String]): Entities = {
    val formatText = sourceText.map(s => "\n\n\n" + s + "\n\n\n")
    val max_seq = formatText.map(_.length).max
    val padd =   formatText.map(s => Vector.fill(max_seq - s.length)(4.0))
    // Convert the sentences into character code Nd4j matrix.
    val input = Nd4j.create(formatText.zipWithIndex.map(s => s._1.map(c => this.char2int.getOrElse(c.toString, this.char2int("<unk>"))).toArray ++ padd(s._2)).toArray)

    this.network.setInput(0, input)
    val results = this.network.feedForward()

    // Gets the matrix from the output layer for batch = b (the batch outputs are inter)
    val out_batch = (m: Array[Array[Float]], b: Int) => m.indices.by(formatText.size).map(_ + b) collect m toArray

    // Take the slice of output removing the \n characters and the padding. For each position take the index of the max output. Get the label of that index.
    val labels = (x: Array[Array[Float]], p: Int, l: List[String]) => x.slice(3, max_seq - (p + 3)).map(r => r.indexWhere(i => i == r.max)).toList.map(o => Try(l(o-1)).getOrElse("O"))

    // Slide through label list and get position where the label type changes. Slide through the resulting list and build the spans as current_position(start), next_position(end), current_label. Remove the "O"s
    val spans = (x: List[String]) => ("" +: x :+ "").sliding(2).zipWithIndex.filter(f => f._1.head != f._1(1)).sliding(2).map(m =>(m.head._2, m(1)._2, m.head._1(1))).filter(_._3 != "O").toList

    // Complete the annotation if the span does not cover the whole token
    val re = """[^a-zA-Z\d]""".r
    val spaces = formatText.map(s => re.findAllMatchIn(s.drop(3).dropRight(3)).map(_.start).toList) // get no-letter characters in sourceText
    val fullSpans = (x: List[(Int,Int,String)], b: Int) => x.map(s => (
      s._1 - Try(spaces(b).map((s._1 - 1) - _).filter(_ >= 0).min).getOrElse(0),
      s._2 + Try(spaces(b).map(_ - s._2).filter(_ >= 0).min).getOrElse(0),
      s._3
    ))

    formatText.indices.map(b => {
      val nonOperators = labels(out_batch(results.get("dense_1").toFloatMatrix, b), padd(b).size, nonOperatorLabels)
      val expOperators = labels(out_batch(results.get("dense_2").toFloatMatrix, b), padd(b).size, operatorLabels)
      val impOperators = labels(out_batch(results.get("dense_3").toFloatMatrix, b), padd(b).size, operatorLabels)
      val nonOperatorsSpan = spans(nonOperators)
      val expOperatorsSpan = spans(expOperators)
      val impOperatorsSpan = spans(impOperators)

      (fullSpans(nonOperatorsSpan, b) ::: fullSpans(expOperatorsSpan, b) ::: fullSpans(impOperatorsSpan, b)).sorted
    }).toList
  }


  private def relation(type1: String, type2: String): Option[String] = {
    Try(Some(this.schema(type1).toList.filter(_._2._2 contains type2).map(_._1).sorted.reverse.head)).getOrElse(None)
  }


  private def linking(entitiy_batches: Entities): Entities = {
    entitiy_batches.map(entities => {
      val links = ListBuffer.empty[(Int, Int, String)]
      val stack = Array(0, 0)
      for ((entity, i) <- entities.zipWithIndex) {
        if (stack(0) != stack(1) && entity._1 - entities(stack(1)-1)._2 > 10)
          stack(0) = stack(1)
        var redays = 0
        for (s <- (stack(0) until stack(1)).toList.reverse) {
          redays += {if (entities(s)._3.startsWith("Day-Of")) 1 else 0}
          if (!(entities(s)._3.startsWith("Day-Of") && redays > 1))
            relation(entities(s)._3, entity._3) match {
              case Some(result) => links += ((s, i, result))
              case None => relation(entity._3, entities(s)._3) match {
                case Some(result) => links += ((i, s, result))
                case None =>
              }
            }
          }
          stack(1) += 1
        }
        links.groupBy(_._1).values.map(_.head).toList
      })
  }


  private def complete(entitiy_batches: Entities, link_batches: Entities, sourceText_batches: List[String]): Properties = {
    (entitiy_batches zip link_batches zip sourceText_batches).map({ case ((entities, links), sourceText) =>
      val properties = {
        for {(entity, i) <- entities.zipWithIndex
             propertyType <- this.schema(entity._3).keys
             property = propertyType match {
                case "Type" =>
                  val p = Try(this.types(entity._3)(sourceText.slice(entity._1, entity._2))).getOrElse(sourceText.slice(entity._1, entity._2)).toString
                  (entity._3, p.last.toString) match {
                    case ("Calendar-Interval", "s") => Some((i, propertyType, p.dropRight(1)))
                    case ("Period", l) if l != "Unknown" && l != "s" => Some((i, propertyType, p + "s"))
                    case _ => Some((i, propertyType, p))
                  }
                case "Value" =>
                  val rgx = """^0+(\d)""".r
                  Some((i, propertyType, WordToNumber.convert(rgx.replaceAllIn(sourceText.slice(entity._1, entity._2), _.group(1)))))
                case intervalttype if intervalttype contains "Interval-Type" =>
                   if (links.exists(l => (l._1 == i || l._2 == i) && (intervalttype contains l._3)))
                     Some((i, propertyType, "Link"))
                  else
                     Some((i, propertyType, "DocTime"))
                case "Semantics" => entity._3 match {
                  case "Last" => Some((i, propertyType, "Interval-Not-Included")) // TODO: Include journal Last
                  case _ => Some((i, propertyType, "Interval-Not-Included"))
                }
                case intervalttype if intervalttype contains "Included" =>
                  Some((i, propertyType, "Included"))
                case _ => None
              }
             if property.isDefined
        } yield property.get
      }
      properties
    })
  }


  private def build(entitiy_batches: Entities, link_batches: Entities, property_batches: Properties): List[Elem] = {
    (entitiy_batches zip link_batches zip property_batches).map({ case ((entities, links), properties) =>
      <data>
        <annotations>
          {for ((entity, e) <- entities.zipWithIndex) yield {
          <entity>
            <id>{s"$e@id"}</id>
            <span>{s"${entity._1},${entity._2}"}</span>
            <type>{entity._3}</type>
            <properties>
              {links.filter(_._1 == e).map(link => <xml>{s"${link._2}@id"}</xml>.copy(label = link._3))}
              {properties.filter(_._1 == e).map(property => <xml>{property._3}</xml>.copy(label = property._2))}
            </properties>
          </entity>
        }}
        </annotations>
      </data>.convert
    })
  }
}
