package org.clulab.timenorm.scfg

import java.time.temporal.ChronoField._
import java.time.temporal.ChronoUnit._

import org.clulab.timenorm.field._
import org.clulab.timenorm.scfg.parse._
import org.clulab.timenorm.{Modifier, PeriodSet}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.Seq

@RunWith(classOf[JUnitRunner])
class SynchronousParserTest extends FunSuite {

  val grammar = SynchronousGrammar.fromString("""ROOTS [Period] [PeriodSet] [TimeSpan] [TimeSpanSet]
    [Nil] ||| a ||| ||| 1.0
    [Nil] ||| the ||| ||| 1.0
    [Nil] ||| . ||| ||| 1.0
    [Nil] ||| very ||| ||| 1.0
    [Nil] ||| just ||| ||| 1.0
    [Int:Ones] ||| one ||| 1 ||| 1.0
    [Int:Ones] ||| two ||| 2 ||| 1.0
    [Int:Ones] ||| three ||| 3 ||| 1.0
    [Int:Tens] ||| twenty ||| 2 ||| 1.0
    [Int] ||| [Int:Ones] ||| [Int:Ones] ||| 1.0
    [Int] ||| [Int:Tens] [Int:Ones] ||| [Int:Tens] [Int:Ones] ||| 1.0
    [Unit:Singular] ||| hour ||| HOURS ||| 1.0
    [Unit:Singular] ||| day ||| DAYS ||| 1.0
    [Unit:Singular] ||| week ||| WEEKS ||| 1.0
    [Unit:Singular] ||| month ||| MONTHS ||| 1.0
    [Unit:Singular] ||| year ||| YEARS ||| 1.0
    [Unit:Singular] ||| decade ||| DECADES ||| 1.0
    [Unit] ||| [Unit:Singular] ||| [Unit:Singular] ||| 1.0
    [Unit:Plural] ||| hours ||| HOURS ||| 1.0
    [Unit:Plural] ||| days ||| DAYS ||| 1.0
    [Unit:Plural] ||| weeks ||| WEEKS ||| 1.0
    [Unit:Plural] ||| months ||| MONTHS ||| 1.0
    [Unit:Plural] ||| years ||| YEARS ||| 1.0
    [Unit:Plural] ||| decades ||| DECADES ||| 1.0
    [Unit] ||| [Unit:Plural] ||| [Unit:Plural] ||| 1.0
    [FieldValue:HourOfAMPM] ||| [Int:1-12] ||| HOUR_OF_AMPM [Int:1-12] ||| 1.0
    [FieldValue:MinuteOfHour] ||| [Int:0-60] ||| MINUTE_OF_HOUR [Int:0-60] ||| 1.0
    [FieldValue:AMPMOfDay] ||| a . m . ||| AMPM_OF_DAY 0 ||| 1.0
    [FieldValue:AMPMOfDay] ||| p . m . ||| AMPM_OF_DAY 1 ||| 1.0
    [FieldValue:PartOfDay] ||| morning ||| MORNING_OF_DAY 1 ||| 1.0
    [FieldValue:DayOfWeek] ||| Thursday ||| DAY_OF_WEEK 4 ||| 1.0
    [FieldValue:WeekendOfWeek] ||| weekend ||| WEEKEND_OF_WEEK 1 ||| 1.0
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
    [FieldValue:MonthOfYear] ||| [Int:1-12] ||| MONTH_OF_YEAR [Int:1-12] ||| 1.0
    [FieldValue:DayOfMonth] ||| [Int:1-31] ||| DAY_OF_MONTH [Int:1-31] ||| 1.0
    [FieldValue:Year] ||| [Int:1900-2100] ||| YEAR [Int:1900-2100] ||| 1.0
    [FieldValue:SeasonOfYear] ||| spring ||| SPRING_OF_YEAR 1 ||| 1.0
    [FieldValue:SeasonOfYear] ||| summer ||| SUMMER_OF_YEAR 1 ||| 1.0
    [FieldValue:SeasonOfYear] ||| fall ||| FALL_OF_YEAR 1 ||| 1.0
    [FieldValue:SeasonOfYear] ||| winter ||| WINTER_OF_YEAR 1 ||| 1.0
    [FieldValue:Easter] ||| easter ||| EASTER_DAY_OF_YEAR 1 ||| 1.0
    [FieldValue:DayQuarter] ||| [FieldValue:DayOfWeek] [FieldValue:PartOfDay] ||| [FieldValue:DayOfWeek] [FieldValue:PartOfDay] ||| 1.0
    [FieldValue:MonthDay] ||| [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| 1.0
    [FieldValue:Time] ||| [FieldValue:HourOfAMPM] : [FieldValue:MinuteOfHour] [FieldValue:AMPMOfDay] ||| [FieldValue:HourOfAMPM] [FieldValue:MinuteOfHour] [FieldValue:AMPMOfDay] ||| 1.0
    [FieldValue:PartialEarlier] ||| [FieldValue:DayQuarter] ||| [FieldValue:DayQuarter] ||| 1.0
    [FieldValue:PartialEarlier] ||| [FieldValue:WeekendOfWeek] ||| [FieldValue:WeekendOfWeek] ||| 1.0
    [FieldValue:PartialEarlier] ||| [FieldValue:MonthDay] ||| [FieldValue:MonthDay] ||| 1.0
    [FieldValue:PartialEarlier] ||| [FieldValue:Easter] ||| [FieldValue:Easter] ||| 1.0
    [FieldValue:PartialEarlier] ||| [FieldValue:Time] ||| [FieldValue:Time] ||| 1.0
    [FieldValue:Date] ||| [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] [FieldValue:Year] ||| [FieldValue:Year] [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| 1.0
    [FieldValue:Date] ||| [FieldValue:DayOfMonth] [FieldValue:MonthOfYear] [FieldValue:Year] ||| [FieldValue:Year] [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| 1.0
    [FieldValue:Date] ||| [FieldValue:Year] [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| [FieldValue:Year] [FieldValue:MonthOfYear] [FieldValue:DayOfMonth] ||| 1.0
    [FieldValue:Absolute] ||| [FieldValue:Year] ||| [FieldValue:Year] ||| 1.0
    [FieldValue:Absolute] ||| [FieldValue:Date] ||| [FieldValue:Date] ||| 1.0
    [FieldValue:Absolute] ||| [FieldValue:Date] [FieldValue:Time] ||| [FieldValue:Date] [FieldValue:Time] ||| 1.0
    [FieldValue:Partial] ||| [FieldValue:SeasonOfYear] ||| [FieldValue:SeasonOfYear] ||| 1.0
    [FieldValue:Partial] ||| [FieldValue:MonthOfYear] ||| [FieldValue:MonthOfYear] ||| 1.0
    [FieldValue:Partial] ||| [FieldValue:DayOfMonth] ||| [FieldValue:DayOfMonth] ||| 1.0
    [FieldValue:Partial] ||| [FieldValue:DayOfWeek] ||| [FieldValue:DayOfWeek] ||| 1.0
    [Period] ||| [Unit:Singular] ||| Simple [Unit:Singular] ||| 1.0
    [Period] ||| [Int] [Unit] ||| Simple [Int] [Unit] ||| 1.0
    [Period] ||| [Unit:Plural] ||| Unspecified [Unit:Plural] ||| 1.0
    [Period] ||| a while ||| Unspecified UNSPECIFIED ||| 1.0
    [Period] ||| [Int,1] [Int,2] / [Int,3] [Unit] ||| Fractional [Int,1] [Int,2] [Int,3] [Unit] ||| 1.0
    [Period] ||| [Period,1] and [Period,2] ||| Sum [Period,1] [Period,2] ||| 1.0
    [Period] ||| less than [Period] ||| WithModifier [Period] LESS_THAN ||| 1.0
    [PeriodSet] ||| daily ||| WithQuantifier ( Period Simple 1 DAYS ) EVERY ||| 1.0
    [PeriodSet] ||| every [Period] ||| WithQuantifier [Period] EVERY ||| 1.0
    [PeriodSet] ||| each [Period] ||| WithQuantifier [Period] EACH ||| 1.0
    [PeriodSet] ||| twice [Period] ||| WithFrequency [Period] 2 ||| 1.0
    [PeriodSet] ||| [Int] [Unit] [Period] ||| WithFrequency [Period] [Int] [Unit] ||| 1.0
    [TimeSpan] ||| now ||| Simple PRESENT ||| 1.0
    [TimeSpan] ||| today ||| FindEnclosing PRESENT DAYS ||| 1.0
    [TimeSpan] ||| this [Unit] ||| FindEnclosing PRESENT [Unit] ||| 1.0
    [TimeSpan] ||| [FieldValue:Absolute] ||| FindAbsolute [FieldValue:Absolute] ||| 1.0
    [TimeSpan] ||| first [Unit] of [TimeSpan] ||| StartAtStartOf [TimeSpan] ( Period Simple 1 [Unit] ) ||| 1.0
    [TimeSpan] ||| tomorrow ||| StartAtEndOf ( TimeSpan FindEnclosing PRESENT DAYS ) ( Period Simple 1 DAYS ) ||| 1.0
    [TimeSpan] ||| next [Period] ||| StartAtEndOf ( TimeSpan FindEnclosing PRESENT [Period] ) [Period] ||| 1.0
    [TimeSpan] ||| [Period] from [TimeSpan] ||| StartAtEndOf ( TimeSpan FindEnclosing [TimeSpan] [Period] ) [Period] ||| 1.0
    [TimeSpan] ||| yesterday ||| EndAtStartOf ( TimeSpan FindEnclosing PRESENT DAYS ) ( Period Simple 1 DAYS ) ||| 1.0
    [TimeSpan] ||| last [Period] ||| EndAtStartOf ( TimeSpan FindEnclosing PRESENT [Period] ) [Period] ||| 1.0
    [TimeSpan] ||| [Period] before [TimeSpan] ||| EndAtStartOf ( TimeSpan FindEnclosing [TimeSpan] [Period] ) [Period] ||| 1.0
    [TimeSpan] ||| [Period] ago ||| MoveEarlier ( TimeSpan WithModifier PRESENT APPROX ) [Period] ||| 1.0
    [TimeSpan] ||| [Period] ago ||| MoveEarlier ( TimeSpan FindEnclosing PRESENT [Period] ) [Period] ||| 1.0
    [TimeSpan] ||| last [FieldValue:Partial] ||| FindEarlier PRESENT [FieldValue:Partial] ||| 1.0
    [TimeSpan] ||| [FieldValue:Partial] ||| FindEarlier PRESENT [FieldValue:Partial] ||| 1.0
    [TimeSpan] ||| [FieldValue:PartialEarlier] ||| FindEarlier PRESENT [FieldValue:PartialEarlier] ||| 1.0
    [TimeSpan] ||| next [FieldValue:Partial] ||| FindLater PRESENT [FieldValue:Partial] ||| 1.0
    [TimeSpan] ||| [FieldValue:Partial] ||| FindLater PRESENT [FieldValue:Partial] ||| 1.0
    [TimeSpan] ||| [FieldValue:Partial] ||| FindAtOrEarlier PRESENT [FieldValue:Partial] ||| 1.0
    [TimeSpan] ||| tonight ||| FindAtOrLater PRESENT ( FieldValue NIGHT_OF_DAY 1 ) ||| 1.0
    [TimeSpan] ||| this [FieldValue:Partial] ||| FindEnclosed ( TimeSpan FindEnclosing PRESENT [FieldValue:Partial] ) [FieldValue:Partial] ||| 1.0
    [TimeSpan] ||| this [FieldValue:PartialEarlier] ||| FindEnclosed ( TimeSpan FindEnclosing PRESENT [FieldValue:PartialEarlier] ) [FieldValue:PartialEarlier] ||| 1.0
    [TimeSpan] ||| early [TimeSpan] ||| WithModifier [TimeSpan] START ||| 1.0
    [TimeSpanSet] ||| Mondays ||| Simple ( FieldValue DAY_OF_WEEK 1 ) ||| 1.0
    [TimeSpanSet] ||| [FieldValue:Partial] nights ||| Simple ( FieldValue [FieldValue:Partial] ( FieldValue NIGHT_OF_DAY 1 ) ) ||| 1.0
    [TimeSpanSet] ||| every [FieldValue:Partial] ||| Simple [FieldValue:Partial] ||| 1.0
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
    this.parser.parseAll(tokens.toIndexedSeq).map(TemporalParse)
  }

  test("parses simple periods") {
    import PeriodParse._
    assert(this.parse("two", "weeks") === Simple(2, WEEKS))
    assert(this.parse("10", "days") === Simple(10, DAYS))
    assert(this.parse("a", "month") === Simple(1, MONTHS))
    assert(this.parse("twenty", "two", "years") === Simple(22, YEARS))
    assert(this.parse("weeks") === Unspecified(WEEKS))
    assert(this.parse("5", "1", "/", "2", "hours") === Fractional(11, 2, HOURS))
    assert(this.parse("a", "while") === Unspecified(UNSPECIFIED))
  }

  test("parses complex periods") {
    import PeriodParse._
    assert(this.parse("two", "weeks", "and", "a", "day") ===
      Sum(Seq(Simple(2, WEEKS), Simple(1, DAYS))))
    assert(this.parse("less", "than", "a", "week") ===
      WithModifier(Simple(1, WEEKS), Modifier.LessThan))
  }
  
  test("parses period sets") {
    import PeriodSetParse._
    assert(this.parse("daily") ===
      WithQuantifier(Simple(PeriodParse.Simple(1, DAYS)), PeriodSet.Quantifier.Every))
    assert(this.parse("every", "week") ===
      WithQuantifier(Simple(PeriodParse.Simple(1, WEEKS)), PeriodSet.Quantifier.Every))
    assert(this.parse("each", "month") ===
      WithQuantifier(Simple(PeriodParse.Simple(1, MONTHS)), PeriodSet.Quantifier.Each))
    assert(this.parse("every", "two", "years") ===
      WithQuantifier(Simple(PeriodParse.Simple(2, YEARS)), PeriodSet.Quantifier.Every))
    assert(this.parse("twice", "a", "month") ===
      WithFrequency(Simple(PeriodParse.Simple(1, MONTHS)), PeriodSet.Frequency(2)))
    assert(this.parse("two", "days", "a", "week") ===
      WithFrequency(Simple(PeriodParse.Simple(1, WEEKS)), PeriodSet.Frequency(2, Some(DAYS))))
  }

  test("parses simple time spans") {
    import TimeSpanParse._
    assert(this.parse("now") === Present)
    assert(this.parse("today") === FindEnclosing(Present, DAYS))
    assert(this.parse("September", "21", "1976") ===
      FindAbsolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)))
    assert(this.parse("9", "21", "1976") ===
      FindAbsolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)))
    assert(this.parse("21", "9", "1976") ===
      FindAbsolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)))
    assert(this.parse("1976", "9", "21") ===
      FindAbsolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)))
    assert(this.parse("October", "15") ===
      FindEarlier(Present, Map(MONTH_OF_YEAR -> 10, DAY_OF_MONTH -> 15)))
    assert(this.parse("10", ":", "35", "a", ".", "m", ".") ===
      FindEarlier(Present, Map(HOUR_OF_AMPM -> 10, MINUTE_OF_HOUR -> 35, AMPM_OF_DAY -> 0)))
    assert(this.parse("last", "summer") ===
      FindEarlier(Present, Map(SUMMER_OF_YEAR -> 1)))
    assert(this.parse("1976", "9", "21", "11", ":", "00", "a", ".", "m", ".") ===
      FindAbsolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21, HOUR_OF_AMPM -> 11, MINUTE_OF_HOUR -> 0, AMPM_OF_DAY -> 0)))
    assert(this.parse("Thursday", "morning") ===
      FindEarlier(Present, Map(DAY_OF_WEEK -> 4, MORNING_OF_DAY -> 1)))
    assert(this.parse("the", "weekend") ===
      FindEarlier(Present, Map(WEEKEND_OF_WEEK -> 1)))
    assert(this.parse("easter") ===
      FindEarlier(Present, Map(EASTER_DAY_OF_YEAR -> 1)))
  }

  test("parses complex time spans") {
    import PeriodParse.{Simple => SimplePeriod}
    import TimeSpanParse._
    
    assert(this.parse("this", "week") === FindEnclosing(Present, WEEKS))
    assert(this.parse("this", "month") === FindEnclosing(Present, MONTHS))
    assert(this.parse("tomorrow") === StartAtEndOf(FindEnclosing(Present, DAYS), SimplePeriod(1, DAYS)))
    assert(this.parse("yesterday") === EndAtStartOf(FindEnclosing(Present, DAYS), SimplePeriod(1, DAYS)))
    assert(this.parse("next", "week") === StartAtEndOf(FindEnclosing(Present, WEEKS), SimplePeriod(1, WEEKS)))
    assert(this.parse("last", "week") === EndAtStartOf(FindEnclosing(Present, WEEKS), SimplePeriod(1, WEEKS)))
    assert(this.parse("next", "month") === StartAtEndOf(FindEnclosing(Present, MONTHS), SimplePeriod(1, MONTHS)))
    assert(this.parseAll("two", "weeks", "ago").toSet === Set(
      MoveEarlier(WithModifier(Present, Modifier.Approx), SimplePeriod(2, WEEKS)),
      MoveEarlier(FindEnclosing(Present, WEEKS), SimplePeriod(2, WEEKS))))
    assert(this.parse("last", "three", "weeks") === EndAtStartOf(FindEnclosing(Present, WEEKS), SimplePeriod(3, WEEKS)))
    
    assert(this.parse("the", "day", "before", "yesterday") ===
      EndAtStartOf(
          FindEnclosing(EndAtStartOf(FindEnclosing(Present, DAYS), SimplePeriod(1, DAYS)), DAYS),
          SimplePeriod(1, DAYS)))
      
    assert(this.parse("next", "October") === FindLater(Present, Map(MONTH_OF_YEAR -> 10)))
    assert(this.parseAll("January").toSet === Set(
        FindEarlier(Present, Map(MONTH_OF_YEAR -> 1)),
        FindAtOrEarlier(Present, Map(MONTH_OF_YEAR -> 1)),
        FindLater(Present, Map(MONTH_OF_YEAR -> 1))))
    assert(this.parse("early", "next", "week") ===
      WithModifier(StartAtEndOf(FindEnclosing(Present, WEEKS), SimplePeriod(1, WEEKS)), Modifier.Start))
    assert(this.parseAll("a", "decade", "ago").toSet === Set(
      MoveEarlier(WithModifier(Present, Modifier.Approx), SimplePeriod(1, DECADES)),
      MoveEarlier(FindEnclosing(Present, DECADES), SimplePeriod(1, DECADES))))
    
    assert(this.parse("tonight") === FindAtOrLater(Present, Map(NIGHT_OF_DAY -> 1)))
    assert(this.parse("first", "week", "of", "2012") ===
      StartAtStartOf(FindAbsolute(Map(YEAR -> 2012)), SimplePeriod(1, WEEKS)))
    assert(this.parse("this", "January") ===
      FindEnclosed(FindEnclosing(Present, YEARS), Map(MONTH_OF_YEAR -> 1)))
    assert(this.parse("this", "10", ":", "30", "p", ".", "m", ".") ===
      FindEnclosed(FindEnclosing(Present, DAYS), Map(HOUR_OF_AMPM -> 10, MINUTE_OF_HOUR -> 30, AMPM_OF_DAY -> 1)))
  }
  
  test("parses time span sets") {
    import TimeSpanSetParse._
    assert(this.parse("Mondays") === Simple(Map(DAY_OF_WEEK -> 1)))
    assert(this.parse("Thursday", "nights") === Simple(Map(DAY_OF_WEEK -> 4, NIGHT_OF_DAY -> 1)))
    assert(this.parse("every", "October") === Simple(Map(MONTH_OF_YEAR -> 10)))
    
  }

  test("parses with nil") {
    import TimeSpanParse._
    assert(this.parse("just", "now") === Present)
    assert(this.parse("this", "week", ".") === FindEnclosing(Present, WEEKS))
    assert(this.parse("this", "very", "month") === FindEnclosing(Present, MONTHS))
    assert(this.parse("the", "next", "October") === FindLater(Present, Map(MONTH_OF_YEAR -> 10)))
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
