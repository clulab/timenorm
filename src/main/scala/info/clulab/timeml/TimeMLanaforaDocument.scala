package info.bethard.timeml

import java.io.File
import scala.io.Source
import com.codecommit.antixml.XML
import com.codecommit.antixml.{text => nodeText }
import java.time.ZonedDateTime
import java.time.LocalDateTime
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeParseException
import com.codecommit.antixml.Elem

object TimeMLanaforaDocument {
  def toZonedDateTimeOption(value: String): Option[ZonedDateTime] = {
    val dateTimeOption =
      try {
        Some(LocalDateTime.parse(value))
      } catch {
        case e: DateTimeParseException =>
          try {
            Some(LocalDate.parse(value).atTime(4, 0))
          } catch {
            case e: DateTimeParseException => None
          }
      }
    dateTimeOption.map(_.atZone(ZoneId.of("UTC")))
  }
}

class TimeMLanaforaDocument(val file: File) {
  import TimeMLanaforaDocument._

  // parse XML, find timex, index times by IDs, and locate the document creation time
  private val root = XML.fromSource(Source.fromFile(file, "US-ASCII"))

  // provide the parsed time expressions
  val timeExpressions = root \\ "entity" map TIMEX

  // map ids to time expressions to allow lookup of anchors
  private val idToTime = timeExpressions.map(e => e.id -> e).toMap

  //val ct: Option[String] =
  timeExpressions(0).functionInDocumentOption = "CREATION_TIME"
  // provide the parsed document creation time
  val Seq(creationTime) = timeExpressions.filter(_.functionInDocumentOption == "CREATION_TIME")
  //val Seq(creationTime) = timeExpressions(0)
}

// a class providing basic TIMEX attributes
case class TIMEX(elem: Elem) {
  val id = elem \ "id" \ nodeText
  val spans: IndexedSeq[(Int, Int)] =
    (elem \ "span" \ nodeText).flatMap(_.split(";")).map(_.split(",").map(_.toInt) match {
      case Array(start, end) => (start, end)
    }).sorted
  val fullSpan: (Int, Int) = (spans.map(_._1).min, spans.map(_._2).max)
  val text = elem \ "properties" \ "value" \ nodeText
  val entityType = elem \ "properties" \ "type" \ nodeText
  val value = elem \ "properties" \ "value" \ nodeText
  val mod = elem  \ "properties" \ "mod" \ nodeText
  var functionInDocumentOption = "None"
}