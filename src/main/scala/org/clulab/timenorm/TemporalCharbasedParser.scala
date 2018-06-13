package org.clulab.timenorm

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.conf.ComputationGraphConfiguration
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.conf.inputs.InputType

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.IndexedSeq
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.codecommit.antixml._
import scala.language.postfixOps

import java.util.Arrays
import java.io.{InputStream, FileInputStream}
import java.time.DateTimeException
import java.time.temporal.IsoFields.QUARTER_YEARS
import java.time.LocalDateTime
import java.time.ZoneOffset

import play.api.libs.json._

import org.clulab.timenorm.formal._
import org.clulab.anafora.{Data, Entity}

object TemporalCharbasedParser {
  private val log: Logger = LoggerFactory.getLogger(TemporalCharbasedParser.getClass)

  def main(args: Array[String]): Unit = {
    val parser = args match {
      case Array(modelPath) =>
        new TemporalCharbasedParser(modelPath)
      case _ =>
        System.err.printf("usage: %s [model-path]\n", this.getClass.getSimpleName)
        System.exit(1)
        throw new IllegalArgumentException
    }


    // use the current date as an anchor
    val now = LocalDateTime.now()
    val anchor = TimeSpan.of(now.getYear, now.getMonthValue, now.getDayOfMonth)
    System.out.printf("Assuming anchor: %s\n", anchor.timeMLValue)
    System.out.println("Type in a time expression (or :quit to exit)")

    // repeatedly prompt for a time expression and then try to parse it
    System.out.print(">>> ")
    for (line <- Source.stdin.getLines.takeWhile(_ != ":quit")) {
      val data: Data = parser.parse(line, anchor)
      println(data.topEntities)
      System.out.print(">>> ")
    }
  }
}


class TemporalCharbasedParser(modelPath: String) {

  private val network: ComputationGraph = KerasModelImport.importKerasModelAndWeights(modelPath, false)
  lazy private val char2int = readDict(this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/char2int.txt"))
  lazy private val unicode2int = readDict(this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/unicate2int.txt"))
  lazy private val operatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/operator.txt")).getLines.toList
  lazy private val nonOperatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/non-operator.txt")).getLines.toList
  lazy private val types = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/linking_configure/date-types.txt")).getLines.map(_.split(' ')).map(a => (a(0), (a(2), a(1)))).toList.groupBy(_._1).mapValues(_.map(_._2).toMap)
  lazy private val schema = (for (es <- scala.xml.XML.load(this.getClass.getResourceAsStream("/org/clulab/timenorm/linking_configure/timenorm-schema.xml")) \\ "entities") yield for (e <- es \ "entity") yield (for (p <- e \\ "property" ) yield ((e \ "@type" head).toString, ((p \ "@type" head).toString, (Try((p \ "@required" head).toString.toBoolean).getOrElse(true), Try((p \ "@instanceOf" head).toString.split(",")).getOrElse(Array()))))) :+  ((e \ "@type" head).toString, ("parentType", (true, Array((es \ "@type" head).toString))))).flatten.flatten.groupBy(_._1).mapValues(_.map(_._2).toMap)

  private val unicodes = Array("Cn", "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Me", "Mc", "Nd", "Nl", "No", "Zs", "Zl", "Zp", "Cc", "Cf", "Cn", "Co", "Cs", "Pd", "Ps", "Pe", "Pc", "Po", "Sm", "Sc", "Sk", "So", "Pi", "Pf")

  private def printModel(){
    println(this.network.summary())
  }

  private def readDict(dictFile: InputStream): Map[String, Double] = {
    try {  Json.parse(dictFile).as[Map[String, Double]] } finally { dictFile.close() }
  }


  def parse(sourceText: String, anchor: TimeSpan): Data = {
    val entities = identification("\n\n\n" + sourceText + "\n\n\n")
    val links = linking(entities)
    val properties = complete(entities, links, sourceText)
    val anafora: Elem = build(entities, links, properties)
    val data = new Data(anafora, Some(sourceText))
    data
  }


  def intervals(data: Data): List[((Int,Int), List[(LocalDateTime, Long)])] = {
    val now = LocalDateTime.now.toString.split("T")(0).split("-").map(_.toInt) match {
      case Array(y, m, d) => SimpleInterval.of(y, m , d)
    }
    val reader = new AnaforaReader(now)(data)
    (for (e <- data.topEntities) yield {
      val span = e.expandedSpan(data)
      val time = Try(reader.temporal(e)(data)).getOrElse(null)
      val timeIntervals: List[(LocalDateTime, Long)] = Try(time match {
        case interval: Interval => List((
          interval.start,
          interval.end.toEpochSecond(ZoneOffset.UTC) - interval.start.toEpochSecond(ZoneOffset.UTC)))
        case intervals: Intervals => (for (interval <- intervals) yield {(
          interval.start,
          interval.end.toEpochSecond(ZoneOffset.UTC) - interval.start.toEpochSecond(ZoneOffset.UTC)
        )}).toList
        case rInterval: RepeatingInterval => List((
          null,
          rInterval.preceding(LocalDateTime.now).next.end.toEpochSecond(ZoneOffset.UTC) - rInterval.preceding(LocalDateTime.now).next.start.toEpochSecond(ZoneOffset.UTC)
        ))
        case period: SimplePeriod => List((
          null,
          period.number * period.unit.getDuration.getSeconds
        ))
        case _ => List((null, 0.toLong))
      }).getOrElse(List((null, 0.toLong)))
      (span, timeIntervals)
    }).toList
  }


  def identification(sourceText: String): List[(Int, Int, String)] = {
    val input0 = Nd4j.create(sourceText.map(c => this.char2int.getOrElse(c.toString(), this.char2int("unknown"))).toArray, Array(1,1,sourceText.length))
    val input1 = Nd4j.create(sourceText.map(c => this.unicode2int.getOrElse(unicodes(c.getType), this.unicode2int("unknown"))).toArray, Array(1,1,sourceText.length))

    this.network.setInput(0, input0)
    this.network.setInput(1, input1)
    val results = this.network.feedForward()

    val labels = (x: Array[Array[Float]], l: List[String]) => (for (r <- x) yield r.indexWhere(i => (i == r.max))).toList.map(o => Try(l(o-1)).getOrElse("O")).drop(3).dropRight(3)

    val spans = (x: List[String]) => ("" +: x :+ "").sliding(2).zipWithIndex.filter(f => f._1(0) != f._1(1)).sliding(2).map(m =>(m(0)._2, m(1)._2, m(0)._1(1))).filter(_._3 != "O").toList

    val re = """[^a-zA-Z\d]""".r
    val spaces = re.findAllMatchIn(sourceText.drop(3).dropRight(3)).map(_.start).toList
    val fullSpans = (x: List[(Int,Int,String)]) => (for (s <- x) yield (
      s._1 - Try(spaces.map((s._1 - 1) - _).filter(_ >= 0).min).getOrElse(0),
      s._2 - Try(spaces.map(_ - s._2).filter(_ >= 0).min).getOrElse(0),
      s._3
    )).toList

    val nonOperators = labels(results.get("timedistributed_1").toFloatMatrix(), nonOperatorLabels)
    val expOperators = labels(results.get("timedistributed_2").toFloatMatrix(), operatorLabels)
    val impOperators = labels(results.get("timedistributed_3").toFloatMatrix(), operatorLabels)
    val nonOperatorsSpan = spans(nonOperators)
    val expOperatorsSpan = spans(expOperators)
    val impOperatorsSpan = spans(impOperators)

    (fullSpans(nonOperatorsSpan) ::: fullSpans(expOperatorsSpan) ::: fullSpans(impOperatorsSpan)).sorted
  }


  def relation(type1: String, type2: String): Option[String] = {
    Try(Some((this.schema(type1) keys).filter(this.schema(type1)(_)._2 contains type2).iterator.next)).getOrElse(None)
  }


  def linking(entities: List[(Int, Int, String)]): List[(Int, Int, String)] = {
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
          relation(entity._3, entities(s)._3) match {
            case Some(result) => links += ((i, s, result))
            case None => relation(entities(s)._3, entity._3) match {
              case Some(result) => links += ((s, i, result))
              case None =>
            }
          }
      }
      stack(1) += 1
    }
    links.groupBy(_._1).values.map(_.head).toList
  }

  def complete(entities: List[(Int, Int, String)], links: List[(Int, Int, String)], sourceText: String): List[(Int, String, String)] = {
    val properties = {
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
          links.find(l => (l._1 == i || l._2 == i) && l._3 == "Interval").isDefined match {
            case false => (i, property, "Link")
            case _ => (i, property, "DocTime")
          }
        }
        case "Semantics" => entity._3 match {
          case "Last" => (i, property, "Interval-Not-Included") // TODO: Include journal Last?1
          case _ => (i, property, "Interval-Not-Included")
        }
        case _ => (-1, "", "")
      }}
    }
    properties
  }
    
  def build(entities: List[(Int, Int, String)], links: List[(Int, Int, String)], properties: List[(Int, String, String)]): Elem = {
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
