package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoField

@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

  val grammar = Grammar.fromString("""
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
    [Anchor] ||| today ||| TODAY ||| 1.0
    [Anchor] ||| yesterday ||| Minus TODAY ( Period 1 DAYS ) ||| 1.0
    [Anchor] ||| tomorrow ||| Plus TODAY ( Period 1 DAYS ) ||| 1.0
    [Anchor] ||| [Field:MonthOfYear] [Field:DayOfMonth] [Field:Year] ||| [Field:Year] [Field:MonthOfYear] [Field:DayOfMonth] ||| 1.0
    [Anchor] ||| [Field:MonthOfYear] [Field:DayOfMonth] ||| [Field:MonthOfYear] [Field:DayOfMonth] ||| 1.0
    [Anchor] ||| next [Period] ||| Plus TODAY [Period] ||| 1.0
    [Anchor] ||| last [Period] ||| Minus TODAY [Period] ||| 1.0
    [Anchor] ||| [Period] from [Anchor] ||| Plus [Anchor] [Period] ||| 1.0
    [Anchor] ||| [Period] before [Anchor] ||| Minus [Anchor] [Period] ||| 1.0
    [Anchor] ||| [Period] ago ||| Minus TODAY [Period] ||| 1.0
    """)

  test("parses simple periods") {
    val parser = new Parser(grammar)
    assert(parser(Seq("two", "weeks")) === Temporal.Period.SimplePeriod(2, ChronoUnit.WEEKS))
    assert(parser(Seq("10", "days")) === Temporal.Period.SimplePeriod(10, ChronoUnit.DAYS))
    assert(parser(Seq("a", "month")) === Temporal.Period.SimplePeriod(1, ChronoUnit.MONTHS))
  }

  test("parses complex periods") {
    val parser = new Parser(grammar)
    assert(parser(Seq("two", "weeks", "and", "a", "day")) ===
      Temporal.Period.Plus(
        Temporal.Period.SimplePeriod(2, ChronoUnit.WEEKS),
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)))
  }

  test("parses simple anchors") {
    val parser = new Parser(grammar)
    assert(parser(Seq("today")) === Temporal.Anchor.Today)
    assert(parser(Seq("September", "21", "1976")) === Temporal.Anchor.Of(Map(
        ChronoField.MONTH_OF_YEAR -> 9,
        ChronoField.DAY_OF_MONTH -> 21,
        ChronoField.YEAR -> 1976)))
    assert(parser(Seq("9", "21", "1976")) === Temporal.Anchor.Of(Map(
        ChronoField.MONTH_OF_YEAR -> 9,
        ChronoField.DAY_OF_MONTH -> 21,
        ChronoField.YEAR -> 1976)))
    assert(parser(Seq("October", "15")) === Temporal.Anchor.Of(Map(
        ChronoField.MONTH_OF_YEAR -> 10,
        ChronoField.DAY_OF_MONTH -> 15)))
  }

  test("parses complex anchors") {
    val parser = new Parser(grammar)
    assert(parser(Seq("tomorrow")) ===
      Temporal.Anchor.Plus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)))
    assert(parser(Seq("yesterday")) ===
      Temporal.Anchor.Minus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)))
    assert(parser(Seq("next", "week")) ===
      Temporal.Anchor.Plus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.WEEKS)))
    assert(parser(Seq("last", "week")) ===
      Temporal.Anchor.Minus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.WEEKS)))
    assert(parser(Seq("next", "month")) ===
      Temporal.Anchor.Plus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.MONTHS)))
    assert(parser(Seq("two", "weeks", "ago")) ===
      Temporal.Anchor.Minus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(2, ChronoUnit.WEEKS)))
    assert(parser(Seq("the", "day", "before", "yesterday")) ===
      Temporal.Anchor.Minus(
        Temporal.Anchor.Minus(
          Temporal.Anchor.Today,
          Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)),
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)))
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
