package info.bethard.timenorm

import java.net.URL
import scala.io.Source
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.DateTimeException

class TimeNormalizer(grammarURL: URL = classOf[TimeNormalizer].getResource("/timenorm.grammar")) {
  private val grammarText = Source.fromURL(grammarURL, "US-ASCII").mkString
  private val grammar = SynchronousGrammar.fromString(grammarText)
  private val sourceSymbols = grammar.sourceSymbols()
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
      // special case for concatenated YYMMDD
      else if (word.matches("^\\d{6}$")) {
        Seq(word.substring(0, 2), "-", word.substring(2, 4), "-", word.substring(4, 6))
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
    // filter out any tokens not in the grammar
    val filteredTokens = tokens.flatten.filter{ token =>
      this.sourceSymbols.contains(token) || SynchronousGrammar.isNumber(token)
    }
    // parse the tokens into TemporalParses
    val parses = this.parser.parseAll(filteredTokens)
    parses.toSeq.map(TemporalParse)
  }

  def normalize(parse: TemporalParse, anchor: TimeSpan): Temporal = {
    parse match {
      case parse: PeriodParse => parse.toPeriod
      case parse: PeriodSetParse => parse.toPeriodSet
      case parse: TimeSpanParse => parse.toTimeSpan(anchor)
      case parse: TimeSpanSetParse => parse.toTimeSpanSet
    }
  }

  def normalize(sourceText: String, anchor: TimeSpan): Option[Temporal] = {
    val parses = this.parseAll(sourceText)
    
    // find only the semantically possible parses
    val temporalTries = for (parse <- parses) yield {
      try {
        Some(this.normalize(parse, anchor))
      } catch {
        case e @ (_: UnsupportedOperationException | _: DateTimeException) => None
      }
    }
    val temporals = temporalTries.flatten
    
    // heuristically pick a temporal when there were multiple parses
    temporals match {
      case Seq() => None
      case Seq(temporal) => Some(temporal)
      case temporals if temporals.size == 2 => {
        val timeSpans = temporals.collect{ case t : TimeSpan => t }
        val periods = temporals.collect{ case p : Period => p }
        // select the earlier of the two time spans
        if (timeSpans.size == 2) {
          Some(timeSpans.minBy(timeSpan => timeSpan.timeMLValueOption match {
            case Some(timeMLValue) => timeMLValue
            case None => throw new UnsupportedOperationException(
              "Don't know how to get value of time span " + timeSpan)
          }))
        }
        // prefer time spans over periods
        else if (periods.size == 1 && timeSpans.size == 1) {
          Some(timeSpans.head)
        }
        // otherwise, give up
        else {
          throw new UnsupportedOperationException(
          "Don't know how to select minimum of:\n%s".format(temporals.mkString("\n")))
        }
      }
      case _ => throw new UnsupportedOperationException(
        "Expected exactly two choices, found:\n%s".format(temporals.mkString("\n")))
    }
  }

}