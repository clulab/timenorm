package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ChronoField._

@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

  val grammar = SynchronousGrammar.fromString("""ROOTS [Period] [Anchor]
    [Number] ||| a ||| 1 ||| 1.0
    [Number] ||| the ||| 1 ||| 1.0
    [Number] ||| two ||| 2 ||| 1.0
    [Number] ||| three ||| 3 ||| 1.0
    [Unit] ||| day ||| DAYS ||| 1.0
    [Unit] ||| days ||| DAYS ||| 1.0
    [Unit] ||| week ||| WEEKS ||| 1.0
    [Unit] ||| weeks ||| WEEKS ||| 1.0
    [Unit] ||| month ||| MONTHS ||| 1.0
    [Unit] ||| months ||| MONTHS ||| 1.0
    [Field:HourOfAMPM] ||| [Number:1-12] ||| HOUR_OF_AMPM [Number:1-12] ||| 1.0
    [Field:MinuteOfHour] ||| [Number:0-60] ||| MINUTE_OF_HOUR [Number:0-60] ||| 1.0
    [Field:AMPMOfDay] ||| a . m . ||| AMPM_OF_DAY 0 ||| 1.0
    [Field:AMPMOfDay] ||| p . m . ||| AMPM_OF_DAY 1 ||| 1.0
    [Field:MonthOfYear] ||| January ||| MONTH_OF_YEAR 1 ||| 1.0
    [Field:MonthOfYear] ||| February ||| MONTH_OF_YEAR 2 ||| 1.0
    [Field:MonthOfYear] ||| March ||| MONTH_OF_YEAR 3 ||| 1.0
    [Field:MonthOfYear] ||| April ||| MONTH_OF_YEAR 4 ||| 1.0
    [Field:MonthOfYear] ||| May ||| MONTH_OF_YEAR 5 ||| 1.0
    [Field:MonthOfYear] ||| June ||| MONTH_OF_YEAR 6 ||| 1.0
    [Field:MonthOfYear] ||| July ||| MONTH_OF_YEAR 7 ||| 1.0
    [Field:MonthOfYear] ||| August ||| MONTH_OF_YEAR 8 ||| 1.0
    [Field:MonthOfYear] ||| September ||| MONTH_OF_YEAR 9 ||| 1.0
    [Field:MonthOfYear] ||| October ||| MONTH_OF_YEAR 10 ||| 1.0
    [Field:MonthOfYear] ||| November ||| MONTH_OF_YEAR 11 ||| 1.0
    [Field:MonthOfYear] ||| December ||| MONTH_OF_YEAR 12 ||| 1.0
    [Field:MonthOfYear] ||| [Number:1-12] ||| MONTH_OF_YEAR [Number:1-12] ||| 1.0
    [Field:DayOfMonth] ||| [Number:1-31] ||| DAY_OF_MONTH [Number:1-31] ||| 1.0
    [Field:Year] ||| [Number:1900-2100] ||| YEAR [Number:1900-2100] ||| 1.0
    [Period] ||| [Unit] ||| [Unit] ||| 1.0
    [Period] ||| [Number] [Unit] ||| [Number] [Unit] ||| 1.0
    [Period] ||| [Period,1] and [Period,2] ||| Sum [Period,1] [Period,2] ||| 1.0
    [Anchor] ||| now ||| NOW ||| 1.0
    [Anchor] ||| today ||| TODAY ||| 1.0
    [Anchor] ||| yesterday ||| Minus TODAY ( Period 1 DAYS ) ||| 1.0
    [Anchor] ||| tomorrow ||| Plus TODAY ( Period 1 DAYS ) ||| 1.0
    [Anchor] ||| this week ||| CurrentField ALIGNED_WEEK_OF_YEAR ||| 1.0
    [Anchor] ||| [Field:MonthOfYear] [Field:DayOfMonth] [Field:Year] ||| Date [Field:Year] [Field:MonthOfYear] [Field:DayOfMonth] ||| 1.0
    [Anchor] ||| [Field:DayOfMonth] [Field:MonthOfYear] [Field:Year] ||| Date [Field:Year] [Field:MonthOfYear] [Field:DayOfMonth] ||| 1.0
    [Anchor] ||| [Field:Year] [Field:MonthOfYear] [Field:DayOfMonth] ||| Date [Field:Year] [Field:MonthOfYear] [Field:DayOfMonth] ||| 1.0
    [Anchor] ||| [Field:MonthOfYear] [Field:DayOfMonth] ||| Previous [Field:MonthOfYear] [Field:DayOfMonth] ||| 1.0
    [Anchor] ||| next [Period] ||| Plus TODAY [Period] ||| 1.0
    [Anchor] ||| last [Period] ||| Minus TODAY [Period] ||| 1.0
    [Anchor] ||| [Period] from [Anchor] ||| Plus [Anchor] [Period] ||| 1.0
    [Anchor] ||| [Period] before [Anchor] ||| Minus [Anchor] [Period] ||| 1.0
    [Anchor] ||| [Period] ago ||| Minus TODAY [Period] ||| 1.0
    [Anchor] ||| last [Field] ||| Previous [Field] ||| 1.0
    [Anchor] ||| next [Field] ||| Next [Field] ||| 1.0
    [Anchor] ||| [Field] ||| CurrentOrPrevious [Field] ||| 1.0
    [Anchor] ||| [Field] ||| Previous [Field] ||| 1.0
    [Anchor] ||| [Field] ||| Next [Field] ||| 1.0
    [Anchor] ||| [Field:HourOfAMPM] : [Field:MinuteOfHour] [Field:AMPMOfDay] ||| Previous [Field:HourOfAMPM] [Field:MinuteOfHour] [Field:AMPMOfDay] ||| 1.0
    """)

  val parser = new SynchronousParser(grammar)

  private def parse(tokens: String*): TemporalParse = {
    this.parseAll(tokens: _*) match {
      case Seq(tree) => tree
      case trees => throw new IllegalArgumentException(
          "Expected one tree, found:\n" + trees.mkString("\n"))
    }
  }

  private def parseAll(tokens: String*): Seq[TemporalParse] = {
    this.parser.parseAll(tokens.toIndexedSeq).map(TemporalParse.apply)
  }

  test("parses simple periods") {
    import PeriodParse._
    assert(this.parse("two", "weeks") === SimplePeriod(2, WEEKS))
    assert(this.parse("10", "days") === SimplePeriod(10, DAYS))
    assert(this.parse("a", "month") === SimplePeriod(1, MONTHS))
  }

  test("parses complex periods") {
    import PeriodParse._
    assert(this.parse("two", "weeks", "and", "a", "day") ===
      Plus(SimplePeriod(2, WEEKS), SimplePeriod(1, DAYS)))
  }

  test("parses simple anchors") {
    import AnchorParse._
    assert(this.parse("now") === Now)
    assert(this.parse("today") === Today)
    assert(this.parse("September", "21", "1976") === Date(1976, 9, 21))
    assert(this.parse("9", "21", "1976") === Date(1976, 9, 21))
    assert(this.parse("21", "9", "1976") === Date(1976, 9, 21))
    assert(this.parse("1976", "9", "21") === Date(1976, 9, 21))
    assert(this.parse("October", "15") === Previous(Map(MONTH_OF_YEAR -> 10, DAY_OF_MONTH -> 15)))
    assert(this.parse("10", ":", "35", "a", ".", "m", ".") ===
      Previous(Map(HOUR_OF_AMPM -> 10, MINUTE_OF_HOUR -> 35, AMPM_OF_DAY -> 0)))
    assert(this.parse("this", "week") === CurrentField(ALIGNED_WEEK_OF_YEAR))
  }

  test("parses complex anchors") {
    import AnchorParse._
    import PeriodParse.SimplePeriod
    assert(this.parse("tomorrow") === Plus(Today, SimplePeriod(1, DAYS)))
    assert(this.parse("yesterday") === Minus(Today, SimplePeriod(1, DAYS)))
    assert(this.parse("next", "week") === Plus(Today, SimplePeriod(1, WEEKS)))
    assert(this.parse("last", "week") === Minus(Today, SimplePeriod(1, WEEKS)))
    assert(this.parse("next", "month") === Plus(Today, SimplePeriod(1, MONTHS)))
    assert(this.parse("two", "weeks", "ago") === Minus(Today, SimplePeriod(2, WEEKS)))
    assert(this.parse("the", "day", "before", "yesterday") ===
      Minus(Minus(Today, SimplePeriod(1, DAYS)), SimplePeriod(1, DAYS)))
    assert(this.parse("next", "October") === Next(Map(MONTH_OF_YEAR -> 10)))
    assert(this.parseAll("January").toSet === Set(
        Previous(Map(MONTH_OF_YEAR -> 1)),
        CurrentOrPrevious(Map(MONTH_OF_YEAR -> 1)),
        Next(Map(MONTH_OF_YEAR -> 1))))
  }

  /*
   * More things to test:
   * 
   * == Anchors ==
   * three days from now
   * a week from yesterday
   * next October 15
   * last Friday the 13th
   * 
   * == Ranges ==
   * the next two days
   */
}
