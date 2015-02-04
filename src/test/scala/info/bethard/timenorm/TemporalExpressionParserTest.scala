package info.bethard.timenorm

import scala.collection.immutable.Seq
import scala.util.Success
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ChronoField._

@RunWith(classOf[JUnitRunner])
class TemporalExpressionParserTest extends FunSuite {

  test("works as described in documentation") {
    // create a new parser (using the default English grammar)
    val parser = new TemporalExpressionParser
    // establish an anchor time
    val anchor = TimeSpan.of(2013, 1, 4)
    // parse an expression given an anchor time (assuming here that it succeeeds)
    val Success(temporal) = parser.parse("ieri", anchor)
    // get the TimeML value ("2012-W51") from the Temporal
    val value = temporal.timeMLValue
    assert(value === "2013-01-03")
  }

  test("produces a single parse when non-identical trees result in identical Temporals") {
    val parser = new TemporalExpressionParser
    val anchor = TimeSpan.of(2013, 1, 3)
    // could be [around [5 years or so]] or [[around 5 years] or so], which are different trees,
    // but which both result in the same Temporal
    assert(parser.parse("circa 5 anni", anchor) === Success(Period(Map(YEARS -> 5), Modifier.Approx)))
  }

  test("parses numbers") {
    val parser = new TemporalExpressionParser
    val anchor = TimeSpan.of(2013, 1, 3)
    //assert(parser.parse("two weeks", anchor) === Success(Period(Map(WEEKS -> 2))))
    //assert(parser.parse("thirteen months", anchor) === Success(Period(Map(MONTHS -> 13))))
    //assert(parser.parse("twenty four days", anchor) === Success(Period(Map(DAYS -> 24))))
    //assert(parser.parse("a hundred weeks", anchor) === Success(Period(Map(WEEKS -> 100))))
    //assert(parser.parse("a hundred six minutes", anchor) === Success(Period(Map(MINUTES -> 106))))
    //assert(parser.parse("a hundred twelve seconds", anchor) === Success(Period(Map(SECONDS -> 112))))
    //assert(parser.parse("a hundred fifty-five years", anchor) === Success(Period(Map(YEARS -> 155))))
    //assert(parser.parse("eight hundred nine decades", anchor) === Success(Period(Map(DECADES -> 809))))
    //assert(parser.parse("two hundred seventeen hours", anchor) === Success(Period(Map(HOURS -> 217))))
    //assert(parser.parse("six hundred eighty seven months", anchor) === Success(Period(Map(MONTHS -> 687))))
    //assert(parser.parse("a thousand days", anchor) === Success(Period(Map(DAYS -> 1000))))
    //assert(parser.parse("a thousand one days", anchor) === Success(Period(Map(DAYS -> 1001))))
    //assert(parser.parse("a thousand ten days", anchor) === Success(Period(Map(DAYS -> 1010))))
    //assert(parser.parse("a thousand twenty days", anchor) === Success(Period(Map(DAYS -> 1020))))
    //assert(parser.parse("a thousand forty-two days", anchor) === Success(Period(Map(DAYS -> 1042))))
    //assert(parser.parse("a thousand one hundred days", anchor) === Success(Period(Map(DAYS -> 1100))))
    //assert(parser.parse("a thousand one hundred ten days", anchor) === Success(Period(Map(DAYS -> 1110))))
    //assert(parser.parse("one thousand one days", anchor) === Success(Period(Map(DAYS -> 1001))))
    //assert(parser.parse("two thousand", anchor) === Success(TimeSpan.fromTimeMLValue("2000")))
    //assert(parser.parse("two thousand one", anchor) === Success(TimeSpan.fromTimeMLValue("2001")))
    //assert(parser.parse("two thousand fifteen", anchor) === Success(TimeSpan.fromTimeMLValue("2015")))
    //assert(parser.parse("two thousand two hundred", anchor) === Success(TimeSpan.fromTimeMLValue("2200")))
    //assert(parser.parse("two thousand two hundred two", anchor) === Success(TimeSpan.fromTimeMLValue("2202")))
    //assert(parser.parse("twenty twenty-three", anchor) === Success(TimeSpan.fromTimeMLValue("2023")))
  }
}
