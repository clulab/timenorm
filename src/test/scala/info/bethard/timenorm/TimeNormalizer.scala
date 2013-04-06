package info.bethard.timenorm

import java.net.URL
import scala.io.Source
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.DateTimeException
import org.threeten.bp.temporal.ISOFields.QUARTER_YEARS

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
    
    // assume that the grammar ambiguity for any expression is at most 2 
    if (parses.size > 2) {
      val message = "Expected no more than 2 parses for \"%s\", found:\n  %s\n"
      System.err.printf(message, sourceText, parses.mkString("\n  "))
    }
    
    // find only the semantically possible parses
    val temporalTries = for (parse <- parses) yield {
      try {
        Some(this.normalize(parse, anchor))
      } catch {
        case e @ (_: UnsupportedOperationException | _: DateTimeException) => None
      }
    }
    val temporals = temporalTries.flatten

    // heuristically pick a temporal from the parses
    val isQuarter = (timeSpan: TimeSpan) => timeSpan.period.unitAmounts.keySet == Set(QUARTER_YEARS)
    val anchorIsQuarter = isQuarter(anchor)
    if (temporals.isEmpty) {
      None
    } else {
      val heuristic = Ordering.fromLessThan[Temporal] {
        // prefer time spans to periods
        case (period: Period, timeSpan: TimeSpan) => false
        case (timeSpan: TimeSpan, period: Period) => true
        // if the anchor is in quarters, prefer a result in quarters
        // otherwise, prefer earlier time spans
        case (timeSpan1: TimeSpan, timeSpan2: TimeSpan) =>
          (anchorIsQuarter, isQuarter(timeSpan1), isQuarter(timeSpan2)) match {
            case (true, true, false) => true
            case (true, false, true) => false
            case _ => timeSpan1.start.isBefore(timeSpan2.start)
          }
        // throw an exception for anything else
        case other => throw new UnsupportedOperationException("Don't know how to order " + other)
      }
      Some(temporals.min(heuristic))
    }
  }
}