package info.bethard.timenorm

import java.io.File
import java.net.URL
import java.util.ArrayList
import java.util.HashSet
import java.util.List
import java.util.Set
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.format.DateTimeParseException
import scala.collection.JavaConverters._
import scala.collection.mutable
import com.codecommit.antixml.XML
import scala.io.Source
import com.codecommit.antixml.text

/**
 * This is not actually a test, but it can be run over TimeML files to see what can and cannot
 * be parsed. To increase coverage, new rules can be added to the timenorm.grammar resource. 
 */
object TimeMLProcessor {

  def main(args: Array[String]): Unit = {
    // load the grammar and create the parser
    val grammarURL = this.getClass.getResource("/timenorm.grammar")
    val grammarText = Source.fromURL(grammarURL, "US-ASCII").mkString
    val grammar = SynchronousGrammar.fromString(grammarText)
    val parser = new TemporalParser(grammar)
    
    // parse TIMEX3 elements from each TimeML file
    for (path <- args; file <- this.allFiles(new File(path))) {
      printf("=== %s ===\n", file)
      val root = XML.fromSource(Source.fromFile(file, "US-ASCII"))
      
      // map times by their IDs (for looking up anchor times)
      val timeElems = root \\ "TIMEX3"
      val idToTimeElem = timeElems.map(e => e.attrs("tid") -> e).toMap
      
      // find the document creation time and its value
      val Seq(dctElem) = timeElems.filter(_.attrs("functionInDocument") == "CREATION_TIME")
      val dctValue = dctElem.attrs("value")
      
      // parse each of the TIMEX3 elements
      for (timeElem <- timeElems) {
        val timeText = (timeElem \\ text).mkString
        val timeValue = timeElem.attrs("value")
        val anchorTimeOption = timeElem.attrs.get("anchorTimeID").map(idToTimeElem)
        val anchorValueOption = anchorTimeOption.map(_.attrs("value"))
        printf(String.format(
          "%s \"%s\" anchor:%s dct:%s\n",
          timeValue,
          timeText,
          anchorValueOption,
          dctValue))

        // parse the expression, using both the DCT and the anchor (if present) as anchor times
        val valueOptions = for {
          anchorValue <- anchorValueOption ++ Seq(dctValue)
          anchorZDT = toZonedDateTime(anchorValue)
          parse <- parser.parseAll(timeText)
        } yield parse match {
            case parse: TimeSpanParse => {
              val timeSpan = parse.toTimeSpan(anchorZDT)
              val timemlValue = timeSpan.timeMLValueOption.getOrElse(
                  "~%s~".format(timeSpan.period.timeMLValue))
              printf(
                "%s %s %s\n",
                timemlValue,
                timeSpan,
                parse)
              timeSpan.timeMLValueOption
            }
            case parse: PeriodParse => {
              val period = parse.toPeriod
              printf("%s %s\n", period.timeMLValue, parse)
              Some(period.timeMLValue)
            }
            case _ => throw new RuntimeException(parse.toString)
        }
        
        // check if the actual value of the TIMEX3 was present in one of the parsed values
        if (!valueOptions.toSet.flatten.contains(timeValue)) {
          println("!!! No correct value !!!")
        }
        println
      }
    }
  }

  def allFiles(fileOrDir: File): Iterator[File] = {
    if (fileOrDir.isDirectory) {
      fileOrDir.listFiles.iterator.map(this.allFiles).flatten
    } else {
      Iterator(fileOrDir)
    }
  }

  def toZonedDateTime(value: String): ZonedDateTime = {
    val anchorLDT =
      try {
        LocalDateTime.parse(value)
      } catch {
        case e: DateTimeParseException => LocalDate.parse(value).atTime(0, 0)
      }
    anchorLDT.atZone(ZoneId.of("UTC"))
  }
}