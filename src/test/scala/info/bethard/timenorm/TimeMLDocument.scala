package info.bethard.timenorm

import java.io.File
import scala.io.Source
import com.codecommit.antixml.XML
import com.codecommit.antixml.text
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.LocalDateTime
import org.threeten.bp.LocalDate
import org.threeten.bp.ZoneId
import org.threeten.bp.format.DateTimeParseException

object TimeMLDocument {
  def toZonedDateTimeOption(value: String): Option[ZonedDateTime] = {
    val dateTimeOption =
      try {
        Some(LocalDateTime.parse(value))
      } catch {
        case e: DateTimeParseException =>
          try {
            Some(LocalDate.parse(value).atTime(0, 0))
          } catch {
            case e: DateTimeParseException => None
          }
      }
    dateTimeOption.map(_.atZone(ZoneId.of("UTC")))
  }
}

class TimeMLDocument(val file: File) {
  import TimeMLDocument._

  // a class providing basic TIMEX attributes
  case class TimeExpression(id: String, text: String, value: String, anchor: Option[ZonedDateTime])

  // parse XML, find timex, index times by IDs, and locate the document creation time
  private val root = XML.fromSource(Source.fromFile(file, "US-ASCII"))
  private val timeElems = root \\ "TIMEX3"
  private val idToTimeElem = timeElems.map(e => e.attrs("tid") -> e).toMap
  private val Seq(creationTimeElem) = timeElems.filter(_.attrs.get("functionInDocument").exists(_ == "CREATION_TIME"))

  // provide the parsed document creation time
  val creationTime: ZonedDateTime = toZonedDateTimeOption(creationTimeElem.attrs("value")).get

  // provide the parsed time expressions
  val timeExpressions: Seq[TimeExpression] = for (timeElem <- timeElems) yield {
    val id = timeElem.attrs("tid")
    val coveredText = (timeElem \\ text).mkString
    val value = timeElem.attrs("value")
    val anchorTimeOption = timeElem.attrs.get("anchorTimeID").map(idToTimeElem)
    val anchorOption = anchorTimeOption.map(_.attrs("value")).map(toZonedDateTimeOption).flatten
    TimeExpression(id, coveredText, value, anchorOption)
  }
}