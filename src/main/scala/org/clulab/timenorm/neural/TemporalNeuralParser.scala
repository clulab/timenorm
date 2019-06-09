package org.clulab.timenorm.neural

import java.io._
import java.nio.file.{Files, Paths}
import java.time.{LocalDateTime, ZoneOffset}

import com.codecommit.antixml._
import org.clulab.anafora.Data
import org.clulab.timenorm.formal._
import org.tensorflow.Graph
import org.tensorflow.Session
import org.tensorflow.Tensor
import org.apache.commons.io.IOUtils
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
              "output" -> s(1)
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
    val sourceText = Source.fromFile(options("input")).getLines.toList
    val annotation = parser.merge(parser.extract(sourceText), sourceText)
    val xml = parser.build(annotation)
    xml.head.writeTo(bw)
    println("Finished!")
    bw.close()
  }
}


case class Span(start: Int, end: Int)
case class Entity(span: Span, label: String)
case class Link(parent: Int, child: Int, relation: String)
case class Property(entity: Int, property: String, value: String)
case class TimeInterval(start: LocalDateTime, end: LocalDateTime, duration: Long)
case class TimeExpression(span: Span, intervals: List[TimeInterval])
case class Annotation(entities: List[List[Entity]], links: List[List[Link]], properties: List[List[Property]])

class TemporalNeuralParser(modelFile: Option[InputStream] = None) {
  private type Entities = List[List[Entity]]
  private type Links = List[List[Link]]
  private type Properties = List[List[Property]]

  lazy private val network = {
    val graph = new Graph()
    graph.importGraphDef(IOUtils.toByteArray(
      modelFile.getOrElse(this.getClass.getResourceAsStream("/org/clulab/timenorm/model/weights-improvement-22.pb"))))
    new Session(graph)
  }
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


  private def readDict(dictFile: InputStream): Map[String, Float] = {
    try {  Json.parse(dictFile).as[Map[String, Float]] } finally { dictFile.close() }
  }

  private def extract_interval(interval: Interval): TimeInterval = TimeInterval(
    interval.start,
    interval.end,
    interval.end.toEpochSecond(ZoneOffset.UTC) - interval.start.toEpochSecond(ZoneOffset.UTC)
  )

  private def cleanText(text: List[String]): List[String] = {
    val antixml_not_allowed = """[^\u0009\u000A\u000D\u0020-\uD7FF\uE000-\uFFFD]+""".r
    text.map(antixml_not_allowed.replaceAllIn(_, " "))
  }

  private def identification(sourceText: List[String]): Entities = {
    val formatText = sourceText.map(s => "\n\n\n" + s + "\n\n\n")
    val max_seq = formatText.map(_.length).max
    val padd =   formatText.map(s => Vector.fill(max_seq - s.length)(4.0.toFloat))
    // Convert the sentences into character code TF Tensor.
    val input = Tensor.create(formatText.zipWithIndex.map(s => s._1.map(c => this.char2int.getOrElse(c.toString, this.char2int("<unk>"))).toArray ++ padd(s._2)).toArray)

    // Run forward pass
    val results = this.network.runner()
      .feed("character",input)
      .fetch("dense_1/truediv").fetch("dense_2/truediv").fetch("dense_3/truediv")
      .run()

    // Take the slice of output removing the \n characters and the padding. For each position take the index of the max output. Get the label of that index.
    val labels = (x: Array[Array[Float]], p: Int, l: List[String]) => x.slice(3, max_seq - (p + 3)).map(r => r.indexWhere(i => i == r.max)).toList.map(o => Try(l(o-1)).getOrElse("O"))

    // Slide through label list and get position where the label type changes. Slide through the resulting list and build the spans as current_position(start), next_position(end), current_label. Remove the "O"s
    val spans = (x: List[String]) => ("" +: x :+ "").sliding(2).zipWithIndex.filter(f => f._1.head != f._1(1)).sliding(2).map(m =>(m.head._2, m(1)._2, m.head._1(1))).filter(_._3 != "O").toList

    // Complete the annotation if the span does not cover the whole token
    val re = """[^a-zA-Z\d]""".r
    val spaces = formatText.map(s => re.findAllMatchIn(s.drop(3).dropRight(3)).map(_.start).toList) // get no-letter characters in sourceText
    val fullSpans = (x: List[(Int,Int,String)], b: Int) => x.map(s => Entity(
      Span(s._1 - Try(spaces(b).map((s._1 - 1) - _).filter(_ >= 0).min).getOrElse(0),
           s._2 + Try(spaces(b).map(_ - s._2).filter(_ >= 0).min).getOrElse(0)),
      s._3
    ))
    
    formatText.indices.map(b => {
      val nonOperators = labels(results.get(0).copyTo(Array.ofDim[Float](results.get(0).shape()(0).toInt, results.get(0).shape()(1).toInt, results.get(0).shape()(2).toInt))(b), padd(b).size, nonOperatorLabels)
      val expOperators = labels(results.get(1).copyTo(Array.ofDim[Float](results.get(1).shape()(0).toInt, results.get(1).shape()(1).toInt, results.get(1).shape()(2).toInt))(b), padd(b).size, operatorLabels)
      val impOperators = labels(results.get(2).copyTo(Array.ofDim[Float](results.get(2).shape()(0).toInt, results.get(2).shape()(1).toInt, results.get(2).shape()(2).toInt))(b), padd(b).size, operatorLabels)
      val nonOperatorsSpan = spans(nonOperators)
      val expOperatorsSpan = spans(expOperators)
      val impOperatorsSpan = spans(impOperators)
      
      (fullSpans(nonOperatorsSpan, b) ::: fullSpans(expOperatorsSpan, b) ::: fullSpans(impOperatorsSpan, b)).sortBy(_.span.start)
    }).toList
  }

  private def relation(type1: String, type2: String): Option[String] = {
    Try(Some(this.schema(type1).toList.filter(_._2._2 contains type2).map(_._1).sorted.reverse.head)).getOrElse(None)
  }

  private def linking(entitiy_batch: Entities): Links = {
    entitiy_batch.map(entities => {
      val links = ListBuffer.empty[Link]
      val stack = Array(0, 0)
      for ((entity, i) <- entities.zipWithIndex) {
        if (stack(0) != stack(1) && entity.span.start - entities(stack(1)-1).span.end > 10)
          stack(0) = stack(1)
        for (s <- (stack(0) until stack(1)).toList.reverse) {
            relation(entities(s).label, entity.label) match {
              case Some(result) => links += Link(s, i, result)
              case None => relation(entity.label, entities(s).label) match {
                case Some(result) => links += Link(i, s, result)
                case None =>
              }
            }
          }
          stack(1) += 1
        }
        links.groupBy(_.parent).values.map(_.head).toList
      })
  }

  private def complete(entitiy_batch: Entities, link_batch: Links, sourceText_batch: List[String]): Properties = {
    (entitiy_batch zip link_batch zip sourceText_batch).map({ case ((entities, links), sourceText) =>
      val properties = {
        for {(entity, i) <- entities.zipWithIndex
             propertyType <- this.schema(entity.label).keys
             property: Option[Property] = propertyType match {
                case "Type" =>
                  val p = Try(this.types(entity.label)(sourceText.slice(entity.span.start, entity.span.end))).getOrElse(sourceText.slice(entity.span.start, entity.span.end)).toString
                  (entity.label, p.last.toString) match {
                    case ("Calendar-Interval", "s") => Some(Property(i, propertyType, p.dropRight(1)))
                    case ("Period", l) if l != "Unknown" && l != "s" => Some(Property(i, propertyType, p + "s"))
                    case _ => Some(Property(i, propertyType, p))
                  }
                case "Value" =>
                  val rgx = """^0*(\d+)[^\d]*$""".r
                  Some(Property(i, propertyType, WordToNumber.convert(rgx.replaceAllIn(sourceText.slice(entity.span.start, entity.span.end), _.group(1)))))
                case intervalttype if intervalttype contains "Interval-Type" =>
                   if (links.exists(l => (l.parent == i || l.child == i) && (intervalttype contains l.relation)))
                     Some(Property(i, propertyType, "Link"))
                  else
                     Some(Property(i, propertyType, "DocTime"))
                case "Semantics" => entity.label match {
                  case "Last" => Some(Property(i, propertyType, "Interval-Not-Included")) // TODO: Include journal Last
                  case _ => Some(Property(i, propertyType, "Interval-Not-Included"))
                }
                case intervalttype if intervalttype contains "Included" =>
                  Some(Property(i, propertyType, "Included"))
                case _ => None
              }
             if property.isDefined
        } yield property.get
      }
      properties
    })
  }

  private def build(annotations: Annotation): List[Elem] = {
    (annotations.entities zip annotations.links zip annotations.properties).map({ case ((entities, links), properties) =>
      <data>
        <annotations>
          {for ((entity, e) <- entities.zipWithIndex) yield {
          <entity>
            <id>{s"$e@id"}</id>
            <span>{s"${entity.span.start},${entity.span.end}"}</span>
            <type>{entity.label}</type>
            <properties>
              {links.filter(_.parent == e).map(link => <xml>{s"${link.child}@id"}</xml>.copy(label = link.relation))}
              {properties.filter(_.entity == e).map(property => <xml>{property.value}</xml>.copy(label = property.property))}
            </properties>
          </entity>
        }}
        </annotations>
      </data>.convert
    })
  }


  def extract(sourceText: List[String], givenEntities: Option[Entities] = None): Annotation = {
    val entities = givenEntities.getOrElse(identification(sourceText))
    val links = linking(entities)
    val properties = complete(entities, links, cleanText(sourceText))
    Annotation(entities, links, properties)
  }

  def parse(sourceText: List[String], givenEntities: Option[Entities] = None): List[Data] =  {
    val annotations = extract(sourceText, givenEntities)
    val xml: List[Elem] = build(annotations)
    val data = (xml zip cleanText(sourceText)).map(b => new Data(b._1, Some(b._2)))
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

  def intervals(data_batch: List[Data], dct: Option[Interval] = Some(UnknownInterval)): List[List[TimeExpression]] = synchronized {
    data_batch.map(data => {
      val reader = new AnaforaReader(dct.get)(data)
      data.topEntities.map(e => {
        val span = e.expandedSpan(data)
        val time = Try(reader.temporal(e)(data)).getOrElse(null)
        val timeIntervals: List[TimeInterval] = Try(time match {
          case interval: Interval => List(extract_interval(interval))
          case intervals: Intervals => intervals.map(extract_interval).toList
          case rInterval: RepeatingInterval => List(TimeInterval(
            null,
            null,
            rInterval.preceding(LocalDateTime.now).next.end.toEpochSecond(ZoneOffset.UTC) - rInterval.preceding(LocalDateTime.now).next.start.toEpochSecond(ZoneOffset.UTC)
          ))
          case period: SimplePeriod => List(TimeInterval(
            null,
            null,
            period.number * period.unit.getDuration.getSeconds
          ))
          case _ => List(TimeInterval(null, null, 0.toLong))
        }).getOrElse(List(TimeInterval(null, null, 0.toLong)))
        TimeExpression(Span(span._1, span._2), timeIntervals)
      }).toList
    })
  }

  def merge(annotation: Annotation, sourceText: List[String]): Annotation = {
    val startOffsets = sourceText.dropRight(1).foldLeft(List(0))({ case (l, s) => s.length + l.head + 1 :: l }).reverse
    val prevNumEntities = annotation.entities.dropRight(1).foldLeft(List(0))({ case (l, e) => e.size + l.head :: l }).reverse
    Annotation(List((annotation.entities zip startOffsets).flatMap { case (b, o) => b.map(e => Entity(Span(e.span.start + o, e.span.end + o), e.label)) }),
              List((annotation.links zip prevNumEntities).flatMap { case (b, n) => b.map{l => Link(l.parent + n, l.child + n, l.relation)}}),
              List((annotation.properties zip prevNumEntities).flatMap { case (b, n) => b.map{p => Property(p.entity + n, p.property, p.value)}}))
  }
}
