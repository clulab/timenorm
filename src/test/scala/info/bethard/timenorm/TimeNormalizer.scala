package info.bethard.timenorm

import java.net.URL
import scala.io.Source
import org.threeten.bp.ZonedDateTime

class TimeNormalizer(grammarURL: URL = classOf[TimeNormalizer].getResource("/timenorm.grammar")) {
  private val grammarText = Source.fromURL(grammarURL, "US-ASCII").mkString
  private val grammar = SynchronousGrammar.fromString(grammarText)
  private val parser = new SynchronousParser(grammar)

  private final val wordBoundary = "\\b|(?<=[^\\p{L}])(?=[\\p{L}])|(?<=[\\p{L}])(?=[^\\p{L}])".r

  def parseAll(sourceText: String): Seq[TemporalParse] = {
    val tokens = for (untrimmedToken <- this.wordBoundary.split(sourceText.toLowerCase())) yield {
      val token = untrimmedToken.trim
      if (token.isEmpty) {
        Seq.empty[String]
      } else if (token.matches("\\d{8}")) {
        // special case for concatenated YYYYMMDD
        Seq(token.substring(0, 4), "-", token.substring(4, 6), "-", token.substring(6, 8))
      } else {
        Seq(token)
      }
    }
    val parses = this.parser.parseAll(tokens.flatten)
    parses.toSeq.map(TemporalParse)
  }
  
  def normalize(parse: TemporalParse, anchor: ZonedDateTime): Either[Period, TimeSpan] = {
    parse match {
      case parse: PeriodParse => Left(parse.toPeriod)
      case parse: TimeSpanParse => Right(parse.toTimeSpan(anchor))
    }
  }
  
  def normalize(parses: Seq[TemporalParse], anchor: ZonedDateTime): Option[Either[Period, TimeSpan]] = {
    val temporals = for (parse <- parses) yield this.normalize(parse, anchor)
    temporals match {
      case Seq() => None
      case Seq(temporal) => Some(temporal)
      case _ => Some(temporals.minBy(_.right.get.timeMLValueOption.get))
    }
  }

}