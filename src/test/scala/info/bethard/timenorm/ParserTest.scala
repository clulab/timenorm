package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ChronoField._

@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

  val grammar = SynchronousGrammar.fromString("""ROOTS [Period] [Time]
    [Nil] ||| a ||| ||| 1.0
    [Nil] ||| the ||| ||| 1.0
    [Nil] ||| . ||| ||| 1.0
    [Nil] ||| very ||| ||| 1.0
    [Nil] ||| just ||| ||| 1.0
    [Number] ||| one ||| 1 ||| 1.0
    [Number] ||| two ||| 2 ||| 1.0
    [Number] ||| three ||| 3 ||| 1.0
    [Unit] ||| day ||| DAYS ||| 1.0
    [Unit] ||| days ||| DAYS ||| 1.0
    [Unit] ||| week ||| WEEKS ||| 1.0
    [Unit] ||| weeks ||| WEEKS ||| 1.0
    [Unit] ||| month ||| MONTHS ||| 1.0
    [Unit] ||| months ||| MONTHS ||| 1.0
    [Unit] ||| year ||| YEARS ||| 1.0
    [Unit] ||| years ||| YEARS ||| 1.0
    [Unit] ||| decade ||| DECADES ||| 1.0
    [Unit] ||| decades ||| DECADES ||| 1.0
    [FieldValue:HourOfAMPM] ||| [Number:1-12] ||| HOUR_OF_AMPM [Number:1-12] ||| 1.0
    [FieldValue:MinuteOfHour] ||| [Number:0-60] ||| MINUTE_OF_HOUR [Number:0-60] ||| 1.0
    [FieldValue:AMPMOfDay] ||| a . m . ||| AMPM_OF_DAY 0 ||| 1.0
    [FieldValue:AMPMOfDay] ||| p . m . ||| AMPM_OF_DAY 1 ||| 1.0
    [FieldValue:MonthOfYear] ||| January ||| MONTH_OF_YEAR 1 ||| 1.0
    [FieldValue:MonthOfYear] ||| February ||| MONTH_OF_YEAR 2 ||| 1.0
    [FieldValue:MonthOfYear] ||| March ||| MONTH_OF_YEAR 3 ||| 1.0
    [FieldValue:MonthOfYear] ||| April ||| MONTH_OF_YEAR 4 ||| 1.0
    [FieldValue:MonthOfYear] ||| May ||| MONTH_OF_YEAR 5 ||| 1.0
    [FieldValue:MonthOfYear] ||| June ||| MONTH_OF_YEAR 6 ||| 1.0
    [FieldValue:MonthOfYear] ||| July ||| MONTH_OF_YEAR 7 ||| 1.0
    [FieldValue:MonthOfYear] ||| August ||| MONTH_OF_YEAR 8 ||| 1.0
    [FieldValue:MonthOfYear] ||| September ||| MONTH_OF_YEAR 9 ||| 1.0
    [FieldValue:MonthOfYear] ||| October ||| MONTH_OF_YEAR 10 ||| 1.0
    [FieldValue:MonthOfYear] ||| November ||| MONTH_OF_YEAR 11 ||| 1.0
    [FieldValue:MonthOfYear] ||| December ||| MONTH_OF_YEAR 12 ||| 1.0
    [FieldValue:MonthOfYear] ||| [Number:1-12] ||| MONTH_OF_YEAR [Number:1-12] ||| 1.0
    [FieldValue:DayOfMonth] ||| [Number:1-31] ||| DAY_OF_MONTH [Number:1-31] ||| 1.0
    [FieldValue:Year] ||| [Number:1900-2100] ||| YEAR [Number:1900-2100] ||| 1.0
    [Period:Simple] ||| [Unit] ||| [Unit] ||| 1.0
    [Period:Simple] ||| [Number] [Unit] ||| [Number] [Unit] ||| 1.0
    [Period:Sum] ||| [Period,1] and [Period,2] ||| [Period,1] [Period,2] ||| 1.0
    [Period:Modifier] ||| less than [Period] ||| LESS_THAN [Period] ||| 1.0
    [Time:Simple] ||| now ||| PRESENT ||| 1.0
    [Time:WithUnit] ||| today ||| PRESENT DAYS ||| 1.0
    [Time:WithUnit] ||| this [Unit] ||| PRESENT [Unit] ||| 1.0
    [Time:Absolute] ||| [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] [FieldValue:Year] ||| [FieldValue:Year] [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| 1.0
    [Time:Absolute] ||| [FieldValue:DayOfMonth] [FieldValue:MonthOfYear] [FieldValue:Year] ||| [FieldValue:Year] [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| 1.0
    [Time:Absolute] ||| [FieldValue:Year] [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| [FieldValue:Year] [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| 1.0
    [Time:Plus] ||| tomorrow ||| PRESENT ( Period:Simple 1 DAYS ) ||| 1.0
    [Time:Plus] ||| next [Unit] ||| PRESENT ( Period:Simple 1 [Unit] ) ||| 1.0
    [Time:Plus] ||| [Period] from [Time] ||| [Time] [Period] ||| 1.0
    [Time:Minus] ||| yesterday ||| PRESENT ( Period:Simple 1 DAYS ) ||| 1.0
    [Time:Minus] ||| last [Unit] ||| PRESENT ( Period:Simple 1 [Unit] ) ||| 1.0
    [Time:Minus] ||| [Period] before [Time] ||| [Time] [Period] ||| 1.0
    [Time:Minus] ||| [Period] ago ||| PRESENT [Period] ||| 1.0
    [Time:Previous] ||| [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| 1.0
    [Time:Previous] ||| last [FieldValue] ||| [FieldValue] ||| 1.0
    [Time:Previous] ||| [FieldValue] ||| [FieldValue] ||| 1.0
    [Time:Previous] ||| [FieldValue:HourOfAMPM] : [FieldValue:MinuteOfHour] [FieldValue:AMPMOfDay] ||| [FieldValue:HourOfAMPM] [FieldValue:MinuteOfHour] [FieldValue:AMPMOfDay] ||| 1.0
    [Time:Next] ||| next [FieldValue] ||| [FieldValue] ||| 1.0
    [Time:Next] ||| [FieldValue] ||| [FieldValue] ||| 1.0
    [Time:CurrentOrPrevious] ||| [FieldValue] ||| [FieldValue] ||| 1.0
    [Time:Modifier] ||| early [Time] ||| START [Time] ||| 1.0
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
    assert(this.parse("two", "weeks") === Simple(2, WEEKS))
    assert(this.parse("10", "days") === Simple(10, DAYS))
    assert(this.parse("a", "month") === Simple(1, MONTHS))
  }

  test("parses complex periods") {
    import PeriodParse._
    assert(this.parse("two", "weeks", "and", "a", "day") ===
      Sum(Seq(Simple(2, WEEKS), Simple(1, DAYS))))
    assert(this.parse("less", "than", "a", "week") ===
      Modifier("LESS_THAN", Simple(1, WEEKS)))
  }

  test("parses simple anchors") {
    import TimeParse._
    assert(this.parse("now") === Present)
    assert(this.parse("today") === WithUnit(Present, DAYS))
    assert(this.parse("September", "21", "1976") ===
      Absolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)))
    assert(this.parse("9", "21", "1976") ===
      Absolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)))
    assert(this.parse("21", "9", "1976") ===
      Absolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)))
    assert(this.parse("1976", "9", "21") ===
      Absolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)))
    assert(this.parse("October", "15") === Previous(Map(MONTH_OF_YEAR -> 10, DAY_OF_MONTH -> 15)))
    assert(this.parse("10", ":", "35", "a", ".", "m", ".") ===
      Previous(Map(HOUR_OF_AMPM -> 10, MINUTE_OF_HOUR -> 35, AMPM_OF_DAY -> 0)))
    assert(this.parse("this", "week") === WithUnit(Present, WEEKS))
    assert(this.parse("this", "month") === WithUnit(Present, MONTHS))
  }

  test("parses complex anchors") {
    import TimeParse._
    import PeriodParse.{ Simple => SimplePeriod }
    assert(this.parse("tomorrow") === Plus(Present, SimplePeriod(1, DAYS)))
    assert(this.parse("yesterday") === Minus(Present, SimplePeriod(1, DAYS)))
    assert(this.parse("next", "week") === Plus(Present, SimplePeriod(1, WEEKS)))
    assert(this.parse("last", "week") === Minus(Present, SimplePeriod(1, WEEKS)))
    assert(this.parse("next", "month") === Plus(Present, SimplePeriod(1, MONTHS)))
    assert(this.parse("two", "weeks", "ago") === Minus(Present, SimplePeriod(2, WEEKS)))
    assert(this.parse("the", "day", "before", "yesterday") ===
      Minus(Minus(Present, SimplePeriod(1, DAYS)), SimplePeriod(1, DAYS)))
    assert(this.parse("next", "October") === Next(Map(MONTH_OF_YEAR -> 10)))
    assert(this.parseAll("January").toSet === Set(
        Previous(Map(MONTH_OF_YEAR -> 1)),
        CurrentOrPrevious(Map(MONTH_OF_YEAR -> 1)),
        Next(Map(MONTH_OF_YEAR -> 1))))
    assert(this.parse("early", "next", "week") ===
      Modifier("START", Plus(Present, SimplePeriod(1, WEEKS))))
    assert(this.parse("a", "decade", "ago") ===
      Minus(Present, SimplePeriod(1, DECADES)))
  }

  test("parses with nil") {
    import TimeParse._
    assert(this.parse("just", "now") === Present)
    assert(this.parse("this", "week", ".") === WithUnit(Present, WEEKS))
    assert(this.parse("this", "very", "month") === WithUnit(Present, MONTHS))
    assert(this.parse("the", "next", "October") === Next(Map(MONTH_OF_YEAR -> 10)))
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
