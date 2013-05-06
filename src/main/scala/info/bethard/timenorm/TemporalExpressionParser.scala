package info.bethard.timenorm

import java.net.URL
import java.util.logging.Logger

import scala.collection.immutable.IndexedSeq
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.threeten.bp.DateTimeException
import org.threeten.bp.temporal.IsoFields.QUARTER_YEARS

class TemporalExpressionParser(grammarURL: URL = classOf[TemporalExpressionParser].getResource("/info/bethard/timenorm/en.grammar")) {
  private val logger = Logger.getLogger(this.getClass.getName)
  private val grammarText = Source.fromURL(grammarURL, "US-ASCII").mkString
  private val grammar = SynchronousGrammar.fromString(grammarText)
  private val sourceSymbols = grammar.sourceSymbols()
  private val parser = new SynchronousParser(grammar)

  private final val wordBoundary = "\\b".r
  private final val letterNonLetterBoundary = "(?<=[^\\p{L}])(?=[\\p{L}])|(?<=[\\p{L}])(?=[^\\p{L}])".r

  def tokenize(sourceText: String): IndexedSeq[String] = {
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
    val filteredTokens = tokens.flatten.filter { token =>
      this.sourceSymbols.contains(token) || SynchronousGrammar.isNumber(token)
    }
    filteredTokens.toIndexedSeq
  }

  def normalize(sourceText: String, anchor: TimeSpan): Try[Temporal] = {
    this.normalizeAndExplain(sourceText, anchor).map(_.head)
  }

  def normalizeAndExplain(sourceText: String, anchor: TimeSpan): Try[Seq[Temporal]] = {
    // tokenize the string
    val tokens = this.tokenize(sourceText)

    // parse the tokens into TemporalParses, failing if there is a syntactic error
    val parsesTry =
      try {
        Success(this.parser.parseAll(tokens).map(TemporalParse))
      } catch {
        case e: UnsupportedOperationException => Failure(e)
      }

    // if there was no syntactic error, convert the TemporalParses to Temporals 
    parsesTry match {
      case Failure(e) => Failure(e)

      case Success(parses) =>
        // assume that the grammar ambiguity for any expression is at most 2 
        if (parses.size > 2) {
          val message = "Expected no more than 2 parses for \"%s\", found:\n  %s"
          this.logger.warning(message.format(sourceText, parses.mkString("\n  ")))
        }

        // try to convert each TemporalParse to a Temporal
        val temporalTries = for (parse <- parses) yield {
          try {
            Success(parse match {
              case parse: PeriodParse => parse.toPeriod
              case parse: PeriodSetParse => parse.toPeriodSet
              case parse: TimeSpanParse => parse.toTimeSpan(anchor)
              case parse: TimeSpanSetParse => parse.toTimeSpanSet
            })
          } catch {
            case e @ (_: UnsupportedOperationException | _: DateTimeException) => Failure(e)
          }
        }

        // if there all TemporalParses had semantic errors, fail
        val temporals = temporalTries.collect { case Success(temporal) => temporal }
        if (temporals.isEmpty) {
          temporalTries.collect { case Failure(e) => Failure(e) }.head
        }
        // otherwise, sort the Temporals by the heuristic
        else {
          Success(temporals.sorted(this.heuristicFor(anchor)))
        }
    }
  }

  // a heuristic for selecting between ambiguous parses
  private def heuristicFor(anchor: TimeSpan): Ordering[Temporal] = {
    val isQuarter = (timeSpan: TimeSpan) => timeSpan.period.unitAmounts.keySet == Set(QUARTER_YEARS)
    val anchorIsQuarter = isQuarter(anchor)
    Ordering.fromLessThan[Temporal] {
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
  }
}