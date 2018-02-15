package org.clulab.timenorm

import scala.util.Success
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.temporal.ChronoUnit._

@RunWith(classOf[JUnitRunner])
class TemporalExpressionParserTest extends FunSuite {

  test("works as described in documentation") {
    // create a new parser (using the default English grammar)
    val parser = new TemporalExpressionParser
    // establish an anchor time
    val anchor = TimeSpan.of(2013, 1, 4)
    // parse an expression given an anchor time (assuming here that it succeeeds)
    val Success(temporal) = parser.parse("two weeks ago", anchor)
    // get the TimeML value ("2012-W51") from the Temporal
    val value = temporal.timeMLValue
    assert(value === "2012-W51")
  }

  test("produces a single parse when non-identical trees result in identical Temporals") {
    val parser = new TemporalExpressionParser
    val anchor = TimeSpan.of(2013, 1, 3)
    // could be [around [5 years or so]] or [[around 5 years] or so], which are different trees,
    // but which both result in the same Temporal
    assert(parser.parse("around 5 years or so", anchor) === Success(Period(Map(YEARS -> 5), Modifier.Approx)))
  }

  test("handles day-of-month with ordinals") {
    val parser = new TemporalExpressionParser
    val anchor = TimeSpan.of(2013, 1, 3)
    assert(parser.parse("November 2nd", anchor) === Success(TimeSpan.of(2012, 11, 2)))
    assert(parser.parse("July 15th", anchor) === Success(TimeSpan.of(2012, 7, 15)))
  }

  test("parses numbers") {
    val parser = new TemporalExpressionParser
    val anchor = TimeSpan.of(2013, 1, 3)
    assert(parser.parse("two weeks", anchor) === Success(Period(Map(WEEKS -> 2))))
    assert(parser.parse("thirteen months", anchor) === Success(Period(Map(MONTHS -> 13))))
    assert(parser.parse("twenty four days", anchor) === Success(Period(Map(DAYS -> 24))))
    assert(parser.parse("a hundred weeks", anchor) === Success(Period(Map(WEEKS -> 100))))
    assert(parser.parse("a hundred six minutes", anchor) === Success(Period(Map(MINUTES -> 106))))
    assert(parser.parse("a hundred twelve seconds", anchor) === Success(Period(Map(SECONDS -> 112))))
    assert(parser.parse("a hundred fifty-five years", anchor) === Success(Period(Map(YEARS -> 155))))
    assert(parser.parse("eight hundred nine decades", anchor) === Success(Period(Map(DECADES -> 809))))
    assert(parser.parse("two hundred seventeen hours", anchor) === Success(Period(Map(HOURS -> 217))))
    assert(parser.parse("six hundred eighty seven months", anchor) === Success(Period(Map(MONTHS -> 687))))
    assert(parser.parse("a thousand days", anchor) === Success(Period(Map(DAYS -> 1000))))
    assert(parser.parse("a thousand one days", anchor) === Success(Period(Map(DAYS -> 1001))))
    assert(parser.parse("a thousand ten days", anchor) === Success(Period(Map(DAYS -> 1010))))
    assert(parser.parse("a thousand twenty days", anchor) === Success(Period(Map(DAYS -> 1020))))
    assert(parser.parse("a thousand forty-two days", anchor) === Success(Period(Map(DAYS -> 1042))))
    assert(parser.parse("a thousand one hundred days", anchor) === Success(Period(Map(DAYS -> 1100))))
    assert(parser.parse("a thousand one hundred ten days", anchor) === Success(Period(Map(DAYS -> 1110))))
    assert(parser.parse("one thousand one days", anchor) === Success(Period(Map(DAYS -> 1001))))
    assert(parser.parse("two thousand", anchor) === Success(TimeSpan.fromTimeMLValue("2000")))
    assert(parser.parse("two thousand one", anchor) === Success(TimeSpan.fromTimeMLValue("2001")))
    assert(parser.parse("two thousand fifteen", anchor) === Success(TimeSpan.fromTimeMLValue("2015")))
    assert(parser.parse("two thousand two hundred", anchor) === Success(TimeSpan.fromTimeMLValue("2200")))
    assert(parser.parse("two thousand two hundred two", anchor) === Success(TimeSpan.fromTimeMLValue("2202")))
    assert(parser.parse("twenty twenty-three", anchor) === Success(TimeSpan.fromTimeMLValue("2023")))
  }

  test("parses Italian numbers") {
    val parser = TemporalExpressionParser.it()
    val anchor1 = TimeSpan.of(2013, 1, 4)
    assert(parser.parse("ieri", anchor1).map(_.timeMLValue) === Success("2013-01-03"))
    assert(parser.parse("circa 5 anni", anchor1) === Success(Period(Map(YEARS -> 5), Modifier.Approx)))
    // examples below were taken from the IT-TimeML guidelines
    // https://sites.google.com/site/ittimeml/
    assert(parser.parse("venerdì due dicembre 2008", anchor1).map(_.timeMLValue) === Success("2008-12-02"))
    assert(parser.parse("07/08/1995", anchor1).map(_.timeMLValue) === Success("1995-08-07"))
    val anchor2 = TimeSpan.of(2008, 11, 28)
    assert(parser.parse("il 3 aprile prossimo", anchor2).map(_.timeMLValue) === Success("2009-04-03"))
    assert(parser.parse("lo scorso 15 maggio", anchor2).map(_.timeMLValue) === Success("2008-05-15"))
    assert(parser.parse("ieri", anchor2).map(_.timeMLValue) === Success("2008-11-27"))
    // guidelines are wrong: "referring to the week from 24-30 November 2008" is 2008-W48, not 2008-W49
    assert(parser.parse("questa settimana", anchor2).map(_.timeMLValue) === Success("2008-W48"))
    assert(parser.parse("al momento", anchor2).map(_.timeMLValue) === Success("PRESENT_REF"))
    assert(parser.parse("passato", anchor2).map(_.timeMLValue) === Success("PAST_REF"))
    assert(parser.parse("futuro", anchor2).map(_.timeMLValue) === Success("FUTURE_REF"))
    assert(parser.parse("ieri alle 16.00", anchor2).map(_.timeMLValue) === Success("2008-11-27T16:00"))
    assert(parser.parse("4 mesi", anchor2).map(_.timeMLValue) === Success("P4M"))
    // guidelines are wrong: should be PT45M, not P45TM
    assert(parser.parse("45 minuti", anchor2).map(_.timeMLValue) === Success("PT45M"))
    // guidelines are wrong: should be P3NI, not PT3NI
    assert(parser.parse("3 notti", anchor2).map(_.timeMLValue) === Success("P3NI"))
    assert(parser.parse("alcuni anni", anchor2).map(_.timeMLValue) === Success("PXY"))
  }

  test("parses day-of-week before full date") {
    val parser = new TemporalExpressionParser
    val anchor = TimeSpan.of(2000, 1, 1)

    assert(parser.parse("Oct. 26, 2012", anchor).map(_.timeMLValue) === Success("2012-10-26"))
    assert(parser.parse("Fri., Oct. 26, 2012", anchor).map(_.timeMLValue) === Success("2012-10-26"))

    assert(parser.parse("May 17 2016", anchor).map(_.timeMLValue) === Success("2016-05-17"))
    assert(parser.parse("Tue May 17 2016", anchor).map(_.timeMLValue) === Success("2016-05-17"))
  }
}
