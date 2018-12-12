package org.clulab.timenorm.neural

import java.io._
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

object TemporalCharbasedParser {
  val usage =
    """
               Usage: TemporalCharbasedParser [options]

                -i <file|directory> | --input <file|directory>
                -o <file|directory> | --output <file|directory>

                -h | --help
                  prints this menu
              """

  def main(args: Array[String]): Unit = {

    def exit() = {
      println(usage)
      sys.exit(1)
    }

    def parseOptions(argList: List[String]): Map[String, String] = {
      if (args.length < 4 || args.length > 4 || args(0) == "-h" || args(0) == "--help") exit()
      argList.sliding(2, 2).map(s =>
        s(0) match {
          case "--input" | "-i" => ("input" -> s(1))
          case "--output" | "-o" => ("output" -> s(1))
          case _ => exit()
        }
      ).toMap
    }

    val options = parseOptions(args.toList)
    val parser = new TemporalCharbasedParser()
    val file = new File(options("output"))
    val bw = new BufferedWriter(new FileWriter(file))
    for ((line, linen) <- Source.fromFile(options("input")).getLines.zipWithIndex) {
      println("Line number " + linen + ": " + line)
      bw.write("Line number: " + linen + "\n")
      val data = parser.parse(line)
      val timexes = parser.intervals(data)
      for ((timex, index) <- timexes.zipWithIndex) {
        bw.write("\tTimEx " + index + ":" + "\n")
        bw.write("\t\tText: " + line.slice(timex._1._1, timex._1._2) + "\n")
        bw.write("\t\tSpan: " + timex._1._1 + " - " + timex._1._2 + "\n")
        bw.write("\t\tIntervals:" + "\n")
        for (interval <- timex._2)
          bw.write("\t\t\t" + interval._1 + "-" + interval._2 + " " + interval._3) + "\n"
      }
      bw.write("\n")
    }
    bw.close()
  }
}


class TemporalCharbasedParser(modelFile: InputStream =
                              getClass.getResource("/org/clulab/timenorm/weights-improvement-22.v2.dl4j.zip").openStream()) {

  private type Entities = List[(Int, Int, String)]
  private type Properties = List[(Int, String, String)]

  lazy private val network: ComputationGraph = ModelSerializer.restoreComputationGraph(modelFile, false)
  lazy private val char2int = readDict(this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/dictionary.json"))
  lazy private val unicode2int = readDict(this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/unicate2int.txt"))
  lazy private val operatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/operator.txt")).getLines.toList
  lazy private val nonOperatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/non-operator.txt")).getLines.toList
  lazy private val unicodes = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/unicodes.txt")).getLines.toList
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


  def parse(sourceText: String): Data = synchronized {
    val entities = identification("\n\n\n" + sourceText  + "\n\n\n")
    val links = linking(entities)
    val antixml_not_allowed = """[^\u0009\u000A\u000D\u0020-\uD7FF\uE000-\uFFFD]+""".r
    val cleanSourceText = antixml_not_allowed.replaceAllIn(sourceText, " ")
    val properties = complete(entities, links, cleanSourceText)
    val anafora: Elem = build(entities, links, properties)
    val data = new Data(anafora, Some(cleanSourceText))
    data
  }


  def dct(data: Data): Interval = {
    val reader = new AnaforaReader(UnknownInterval)(data)
    val time = Try(reader.temporal(data.topEntities(0))(data)).getOrElse(null)
    time match {
      case interval: Interval => interval
      case intervals: Intervals => intervals(0)
      case _ => UnknownInterval
    }
  }


  def extract_interval(interval: Interval): (LocalDateTime, LocalDateTime, Long) = (
      interval.start,
      interval.end,
      interval.end.toEpochSecond(ZoneOffset.UTC) - interval.start.toEpochSecond(ZoneOffset.UTC)
  )


  def intervals(data: Data, dct: Option[Interval] = Some(UnknownInterval)): List[((Int,Int), List[(LocalDateTime, LocalDateTime, Long)])] = synchronized {
    val reader = new AnaforaReader(dct.get)(data)
    (for (e <- data.topEntities) yield {
      val span = e.expandedSpan(data)
      val time = Try(reader.temporal(e)(data)).getOrElse(null)
      val timeIntervals: List[(LocalDateTime, LocalDateTime, Long)] = Try(time match {
        case interval: Interval => List(extract_interval(interval))
        case intervals: Intervals => (for (interval <- intervals) yield {extract_interval(interval)}).toList
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
  }


  private def identification(sourceText: String): Entities = {
    val max_seq = 356
    val padd = Vector.fill(max_seq - sourceText.length)(4.0)
    val input = Nd4j.create((sourceText.map(c => this.char2int.getOrElse(c.toString(), this.char2int("<unk>"))) ++ padd).toArray, Array(1,max_seq))

    this.network.setInput(0, input)
    val results = this.network.feedForward()

    // Take the slice of output removing the \n characters and the padding. For each position take the index of the max output. Get the label of that index.
    val labels = (x: Array[Array[Float]], l: List[String]) => (for (r <- x.slice(3, max_seq - (padd.size + 3))) yield r.indexWhere(i => (i == r.max))).toList.map(o => Try(l(o-1)).getOrElse("O"))

    // Slide through label list and get position where the label type changes. Slide through the resulting list and build the spans as current_position(start), next_position(end), current_label. Remove the "O"s
    val spans = (x: List[String]) => ("" +: x :+ "").sliding(2).zipWithIndex.filter(f => f._1(0) != f._1(1)).sliding(2).map(m =>(m(0)._2, m(1)._2, m(0)._1(1))).filter(_._3 != "O").toList

    // Complete the annotation if the span does not cover the whole token
    val re = """[^a-zA-Z\d]""".r
    val spaces = re.findAllMatchIn(sourceText.drop(3).dropRight(3)).map(_.start).toList // get no-letter characters in sourceText
    val fullSpans = (x: List[(Int,Int,String)]) => (for (s <- x) yield (
      s._1 - Try(spaces.map((s._1 - 1) - _).filter(_ >= 0).min).getOrElse(0),
      s._2 + Try(spaces.map(_ - s._2).filter(_ >= 0).min).getOrElse(0),
      s._3
    ))

    val nonOperators = labels(results.get("dense_1").toFloatMatrix(), nonOperatorLabels)
    val expOperators = labels(results.get("dense_2").toFloatMatrix(), operatorLabels)
    val impOperators = labels(results.get("dense_3").toFloatMatrix(), operatorLabels)
    val nonOperatorsSpan = spans(nonOperators)
    val expOperatorsSpan = spans(expOperators)
    val impOperatorsSpan = spans(impOperators)

    (fullSpans(nonOperatorsSpan) ::: fullSpans(expOperatorsSpan) ::: fullSpans(impOperatorsSpan)).sorted
  }


  private def relation(type1: String, type2: String): Option[String] = {
    Try(Some(this.schema(type1).toList.filter(_._2._2 contains type2).map(_._1).sorted.reverse.head)).getOrElse(None)
  }


  private def linking(entities: Entities): Entities = {
    val links = ListBuffer.empty[(Int, Int, String)]
    var stack = Array(0, 0)
    for ((entity, i) <- entities.zipWithIndex) {
      if (stack(0) != stack(1) && entity._1 - entities(stack(1)-1)._2 > 10)
        stack(0) = stack(1)
      var redays = 0
      for (s <- (stack(0) to stack(1) - 1).toList.reverse) {
        redays += { entities(s)._3.startsWith("Day-Of") match {
          case true => 1
          case _ => 0
        }}
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
  }


  private def complete(entities: Entities, links: Entities, sourceText: String): Properties = {
    for ((entity, i) <- entities.zipWithIndex;
      property <- this.schema(entity._3).keys) yield { property match {
        case "Type" => {
          val p = Try(this.types(entity._3)(sourceText.slice(entity._1, entity._2))).getOrElse(sourceText.slice(entity._1, entity._2)).toString
          (entity._3, p.last.toString) match {
            case ("Calendar-Interval", "s") => (i, property, p.dropRight(1))
            case ("Period", l) if l != "Unknown" && l != "s" => (i, property, p + "s")
            case _ => (i, property, p)
          }
        }
        case "Value" => {
          val rgx = """^0+(\d)""".r
          (i, property, WordToNumber.convert(rgx.replaceAllIn(sourceText.slice(entity._1, entity._2), _.group(1))))
        }
        case intervalttype if intervalttype contains "Interval-Type" => {
          links.find(l => (l._1 == i || l._2 == i) && (intervalttype contains l._3)).isDefined match {
            case true => (i, property, "Link")
            case _ => (i, property, "DocTime")
          }
        }
        case "Semantics" => entity._3 match {
          case "Last" => (i, property, "Interval-Not-Included") // TODO: Include journal Last?1
          case _ => (i, property, "Interval-Not-Included")
        }
        case intervalttype if intervalttype contains "Included" => {
          (i, property, "Included")
        }
        case _ => (-1, "", "")
      }
    }
  }


  private def build(entities: Entities, links: Entities, properties: Properties): Elem = {
    <data>
    <annotations>
    {
      for ((entity, e) <- entities.zipWithIndex) yield {
        <entity>
        <id>{s"${e}@id"}</id>
        <span>{s"${entity._1},${entity._2}"}</span>
        <type>{entity._3}</type>
        <properties>
        {
          links.filter(_._1 == e).map(link => <xml>{s"${link._2}@id"}</xml>.copy(label = link._3))
        }
        {
          properties.filter(_._1 == e).map(property => <xml>{property._3}</xml>.copy(label = property._2))
        }
        </properties>
        </entity>
      }
    }
    </annotations>
    </data>.convert
  }
}
