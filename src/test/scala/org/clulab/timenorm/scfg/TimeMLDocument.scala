package org.clulab.timenorm.scfg

import java.io.File
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeParseException

import scala.xml.{Elem, XML}

object TimeMLDocument {
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

class TimeMLDocument(val file: File) {

  // a class providing basic TIMEX attributes
  case class TimeExpression(elem: Elem) {
    val Some(id: String) = elem.attributes.get("tid").map(_.text)
    val text: String = elem.text
    val Some(value: String) = elem.attributes.get("value").map(_.text)
    val functionInDocumentOption: Option[String] = elem.attributes.get("functionInDocument").map(_.text)
    val anchorIDOption: Option[String] = elem.attributes.get("anchorTimeID").map(_.text)
    def anchor: Option[TimeExpression] = anchorIDOption.map(idToTime)
  }

  // parse XML, find timex, index times by IDs, and locate the document creation time
  private val root = XML.loadFile(file)
  
  // provide the parsed time expressions
  val timeExpressions: Seq[TimeExpression] = root \\ "TIMEX3" map {
    case elem: Elem => TimeExpression(elem)
  }
  
  // map ids to time expressions to allow lookup of anchors 
  private val idToTime = timeExpressions.map(e => e.id -> e).toMap

  // provide the parsed document creation time
  val Seq(creationTime: TimeExpression) = timeExpressions.filter(_.functionInDocumentOption.contains("CREATION_TIME"))
}