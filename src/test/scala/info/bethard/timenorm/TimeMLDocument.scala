package info.bethard.timenorm

import java.io.File

import scala.collection.mutable
import scala.io.Source

import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.format.DateTimeParseException

import com.codecommit.antixml.CDATA
import com.codecommit.antixml.Elem
import com.codecommit.antixml.EntityRef
import com.codecommit.antixml.ProcInstr
import com.codecommit.antixml.Text
import com.codecommit.antixml.XML
import com.codecommit.antixml.{text => nodeText}

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
  import TimeMLDocument._

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
  
  val text = (root \\ nodeText).mkString
  
  val offsets = {
    val map = mutable.Map.empty[Elem, (Int, Int)]
    this.fillOffsets(this.root, 0, map)
    map.toMap
  }

  private def fillOffsets(elem: Elem, start: Int, map: mutable.Map[Elem, (Int, Int)]): Int = {
    var curr = start
    for (child <- elem.children) child match {
      case elem: Elem => curr = this.fillOffsets(elem, curr, map)
      case cdata: CDATA => curr += cdata.text.length
      case text: Text => curr += text.text.length
      case entityRef: EntityRef => throw new UnsupportedOperationException
      case procInstr: ProcInstr => throw new UnsupportedOperationException
    }
    map += elem -> (start, curr)
    curr
  }
  
  // provide the parsed time expressions
  val timeExpressions = root \\ "TIMEX3" map TimeExpression
  
  // map ids to time expressions to allow lookup of anchors 
  private val idToTime = timeExpressions.map(e => e.id -> e).toMap

  // provide the parsed document creation time
  val Seq(creationTime) = timeExpressions.filter(_.functionInDocumentOption == Some("CREATION_TIME"))
}