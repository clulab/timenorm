package info.bethard.timenorm

import java.net.URL
import scala.io.Source
import org.threeten.bp.ZonedDateTime

class TimeNormalizer(grammarURL: URL = classOf[TimeNormalizer].getResource("/timenorm.grammar")) {
  private val grammarText = Source.fromURL(grammarURL, "US-ASCII").mkString
  private val grammar = SynchronousGrammar.fromString(grammarText)
  private val parser = new SynchronousParser(grammar)

  private final val wordBoundary = "\\b".r
  private final val letterNonLetterBoundary = "(?<=[^\\p{L}])(?=[\\p{L}])|(?<=[\\p{L}])(?=[^\\p{L}])".r

  def parseAll(sourceText: String): Seq[TemporalParse] = {
    val tokens = for (untrimmedWord <- this.wordBoundary.split(sourceText)) yield {
      val word = untrimmedWord.trim
      if (word.isEmpty) {
        Seq.empty[String]
      }
      // special case for concatenated YYYYMMDD
      else if (word.matches("^\\d{8}$")) {
        Seq(word.substring(0, 4), "-", word.substring(4, 6), "-", word.substring(6, 8))
      }
      // special case for concatenated HHMMTZ
      else if (word.matches("^\\d{4}[A-Z]{3,4}$")) {
        Seq(word.substring(0, 2), ":", word.substring(2, 4), word.substring(4).toLowerCase)
      }
      // otherwise, split at all letter/non-letter boundaries
      else {
        this.letterNonLetterBoundary.split(word).toSeq.map(_.trim.toLowerCase).filterNot(_.isEmpty)
      }
    }
    val parses = this.parser.parseAll(tokens.flatten)
    parses.toSeq.map(TemporalParse)
  }

  def normalize(parse: TemporalParse, anchor: ZonedDateTime): Temporal = {
    parse match {
      case parse: PeriodParse => parse.toPeriod
      case parse: PeriodSetParse => parse.toPeriodSet
      case parse: TimeSpanParse => parse.toTimeSpan(anchor)
      case parse: TimeSpanSetParse => parse.toTimeSpanSet
    }
  }

  def normalize(parses: Seq[TemporalParse], anchor: ZonedDateTime): Option[Temporal] = {
    val temporals = for (parse <- parses) yield this.normalize(parse, anchor)
    temporals match {
      case Seq() => None
      case Seq(temporal) => Some(temporal)
      case temporals if temporals.size == 2 => Some(temporals.minBy {
        case timeSpan: TimeSpan => timeSpan.timeMLValueOption match {
          case Some(timeMLValue) => timeMLValue
          case None => throw new UnsupportedOperationException(
            "Don't know how to get value of time span " + timeSpan)
        }
        case _ => throw new UnsupportedOperationException(
          "Don't know how to select minimum of:\n%s".format(temporals.mkString("\n")))
      })
      case _ => throw new UnsupportedOperationException(
        "Expected exactly two choices, found:\n%s".format(temporals.mkString("\n")))
    }
  }

}