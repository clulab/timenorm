package org.clulab.timenorm.scfg

import java.io.File
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeParseException

import com.codecommit.antixml.{Elem, XML, text => nodeText}

import scala.io.Source

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
    val id = elem.attrs("tid")
    val text = (elem \\ nodeText).mkString
    val value = elem.attrs("value")
    val functionInDocumentOption = elem.attrs.get("functionInDocument")
    val anchorIDOption = elem.attrs.get("anchorTimeID")
    def anchor = anchorIDOption.map(idToTime)
  }

  // parse XML, find timex, index times by IDs, and locate the document creation time
  private val root = XML.fromSource(Source.fromFile(file, "US-ASCII"))
  
  // provide the parsed time expressions
  val timeExpressions = root \\ "TIMEX3" map TimeExpression
  
  // map ids to time expressions to allow lookup of anchors 
  private val idToTime = timeExpressions.map(e => e.id -> e).toMap

  // provide the parsed document creation time
  val Seq(creationTime) = timeExpressions.filter(_.functionInDocumentOption == Some("CREATION_TIME"))
}