package org.clulab.timenorm.scfg.parse

import java.time.DayOfWeek
import java.time.temporal.ChronoField._
import java.time.temporal.ChronoUnit._
import java.time.temporal.IsoFields._
import java.time.temporal.TemporalUnit

import org.clulab.timenorm.{Modifier, TimeSpan}
import org.clulab.timenorm.field._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.Seq

@RunWith(classOf[JUnitRunner])
class TemporalParseTest extends FunSuite {

  test("resolves simple periods") {
    import PeriodParse._
    assertPeriod(Simple(3, CENTURIES), "P3CE", CENTURIES -> 3)
    assertPeriod(Simple(2, WEEKS), "P2W", WEEKS -> 2)
    assertPeriod(Simple(10, DAYS), "P10D", DAYS -> 10)
    assertPeriod(Simple(1, MONTHS), "P1M", MONTHS -> 1)
    assertPeriod(Simple(16, HOURS), "PT16H", HOURS -> 16)
    assertPeriod(Simple(20, MINUTES), "PT20M", MINUTES -> 20)
    assertPeriod(Simple(53, SECONDS), "PT53S", SECONDS -> 53)
    assertPeriod(Unspecified(DECADES), "PXDE", DECADES -> Int.MaxValue)
    assertPeriod(Unspecified(WEEKS), "PXW", WEEKS -> Int.MaxValue)
    assertPeriod(Unspecified(MORNINGS), "PXMO", MORNINGS -> Int.MaxValue)
    assertPeriod(Unspecified(SECONDS), "PTXS", SECONDS -> Int.MaxValue)
    assertPeriod(Fractional(11, 2, HOURS), "PT5H30M", HOURS -> 5, MINUTES -> 30)
    assertPeriod(Fractional(3, 2, WEEKS), "P1W3DT12H", WEEKS -> 1, DAYS -> 3, HOURS -> 12)
  }

  test("resolves complex periods") {
    import PeriodParse._
    assertPeriod(
        Sum(Seq(Simple(2, WEEKS), Simple(1, DAYS))),
        "P2W1D", WEEKS -> 2, DAYS -> 1)
    assertPeriod(
        Sum(Seq(Simple(2, DAYS), Simple(1, DAYS))),
        "P3D", DAYS -> 3)
    assertPeriod(
        Sum(Seq(Simple(2, DECADES), Simple(3, CENTURIES))),
        "P32DE", DECADES -> 2, CENTURIES -> 3)
    assertPeriod(
        Sum(Seq(Simple(191, YEARS), Simple(2, DECADES), Simple(3, CENTURIES))),
        "P511Y", YEARS -> 191, DECADES -> 2, CENTURIES -> 3)
  }

  private def assertPeriod(
    periodParse: PeriodParse,
    timeMLValue: String,
    unitAmounts: (TemporalUnit, Int)*) = {
    val period = periodParse.toPeriod
    assert(period.timeMLValue === timeMLValue)
    assert(period.unitAmounts === unitAmounts.toMap)
  }

  val now = TimeSpan.of(2012, 12, 12, 12, 12, 12)
  val nowStart = now.start.toLocalDateTime.toString
  val nowEnd = now.end.toLocalDateTime.toString

  test("resolves simple time spans") {
    import TimeSpanParse._
    assertTimeSpan(
      Past,
      "-999999999-01-01T00:00", nowStart, "PXX", "PAST_REF", "APPROX")
    assertTimeSpan(
      Present,
      nowStart, nowEnd, "PT1S", "PRESENT_REF")
    assertTimeSpan(
      Future,
      nowEnd, "+999999999-12-31T23:59:59.999999999", "PXX", "FUTURE_REF", "APPROX")
    assertTimeSpan(
      FindAbsolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)),
      "1976-09-21T00:00", "1976-09-22T00:00", "P1D", "1976-09-21")
    assertTimeSpan(
      FindAbsolute(Map(YEAR -> 2030)),
      "2030-01-01T00:00", "2031-01-01T00:00", "P1Y", "2030")
    assertTimeSpan(
      FindAtOrEarlier(Present, Map(YEAR_OF_CENTURY -> 76, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)),
      "1976-09-21T00:00", "1976-09-22T00:00", "P1D", "1976-09-21")
    assertTimeSpan(
      FindEarlier(Present, Map(MONTH_OF_YEAR -> 10, DAY_OF_MONTH -> 15)),
      "2012-10-15T00:00", "2012-10-16T00:00", "P1D", "2012-10-15")
    assertTimeSpan(
      FindLater(Present, Map(MONTH_OF_YEAR -> 2)),
      "2013-02-01T00:00", "2013-03-01T00:00", "P1M", "2013-02")
    assertTimeSpan(
      FindLater(Present, Map(DAY_OF_WEEK -> DayOfWeek.FRIDAY.getValue, DAY_OF_MONTH -> 13)),
      "2013-09-13T00:00", "2013-09-14T00:00", "P1D", "2013-09-13")
    assertTimeSpan(
      FindEarlier(Present, Map(CLOCK_HOUR_OF_AMPM -> 11, MINUTE_OF_HOUR -> 18, AMPM_OF_DAY -> 0)),
      "2012-12-12T11:18", "2012-12-12T11:19", "PT1M", "2012-12-12T11:18")
    assertTimeSpan(
      FindEarlier(Present, Map(SPRING_OF_YEAR -> 1)),
      "2012-03-20T00:00", "2012-06-21T00:00", "P1SP", "2012-SP")
    assertTimeSpan(
      FindLater(Present, Map(SUMMER_OF_YEAR -> 1)),
      "2013-06-21T00:00", "2013-09-22T00:00", "P1SU", "2013-SU")
    assertTimeSpan(
      FindEarlier(Present, Map(FALL_OF_YEAR -> 1)),
      "2011-09-22T00:00", "2011-12-21T00:00", "P1FA", "2011-FA")
    assertTimeSpan(
      FindLater(Present, Map(WINTER_OF_YEAR -> 1)),
      "2012-12-21T00:00", "2013-03-20T00:00", "P1WI", "2012-WI")
    assertTimeSpan(
      FindAtOrEarlier(Present, Map(DAY_OF_WEEK -> DayOfWeek.WEDNESDAY.getValue)),
      "2012-12-12T00:00", "2012-12-13T00:00", "P1D", "2012-12-12")
    assertTimeSpan(
      FindAtOrEarlier(Present, Map(DAY_OF_WEEK -> DayOfWeek.TUESDAY.getValue)),
      "2012-12-11T00:00", "2012-12-12T00:00", "P1D", "2012-12-11")
    assertTimeSpan(
      FindEarlier(Present, Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      "2011-12-12T00:00", "2011-12-13T00:00", "P1D", "2011-12-12")
    assertTimeSpan(
      FindAtOrEarlier(Present, Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      "2012-12-12T00:00", "2012-12-13T00:00", "P1D", "2012-12-12")
    assertTimeSpan(
      FindLater(Present, Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      "2013-12-12T00:00", "2013-12-13T00:00", "P1D", "2013-12-12")
    assertTimeSpan(
      FindEnclosing(Present, DAYS),
      "2012-12-12T00:00", "2012-12-13T00:00", "P1D", "2012-12-12")
    assertTimeSpan(
      FindEnclosing(Present, WEEKS),
      "2012-12-10T00:00", "2012-12-17T00:00", "P1W", "2012-W50")
    assertTimeSpan(
      FindEarlier(Present, Map(MORNING_OF_DAY -> 1)),
      "2012-12-12T00:00", "2012-12-12T12:00", "P1MO", "2012-12-12TMO")
    assertTimeSpan(
      FindLater(Present, Map(AFTERNOON_OF_DAY -> 1)),
      "2012-12-13T12:00", "2012-12-13T18:00", "P1AF", "2012-12-13TAF")
    assertTimeSpan(
      FindEarlier(Present, Map(EVENING_OF_DAY -> 1)),
      "2012-12-11T17:00", "2012-12-12T00:00", "P1EV", "2012-12-11TEV")
    assertTimeSpan(
      FindAtOrLater(Present, Map(NIGHT_OF_DAY -> 1)),
      "2012-12-12T21:00", "2012-12-13T04:00", "P1NI", "2012-12-12TNI")
    assertTimeSpan(
      FindEarlier(Present, Map(NIGHT_OF_DAY -> 1)),
      "2012-12-11T21:00", "2012-12-12T04:00", "P1NI", "2012-12-11TNI")
    assertTimeSpan(
      FindEnclosing(Present, AFTERNOONS),
      "2012-12-12T12:00", "2012-12-12T18:00", "P1AF", "2012-12-12TAF")
    assertTimeSpan(
      FindLater(Present, Map(WEEKEND_OF_WEEK -> 1)),
      "2012-12-15T00:00", "2012-12-17T00:00", "P1WE", "2012-W50-WE")
    assertTimeSpan(
      FindLater(Present, Map(EASTER_DAY_OF_YEAR -> 1)),
      "2013-03-31T00:00", "2013-04-01T00:00", "P1D", "2013-03-31")
    assertTimeSpan(
      FindEarlier(Present, Map(EASTER_DAY_OF_YEAR -> 1)),
      "2012-04-08T00:00", "2012-04-09T00:00", "P1D", "2012-04-08")
    assertTimeSpan(
      FindEnclosing(Present, QUARTER_YEARS),
      "2012-10-01T00:00", "2013-01-01T00:00", "P1Q", "2012-Q4")
    assertTimeSpan(
      FindEarlier(Present, Map(QUARTER_OF_YEAR -> 1)),
      "2012-01-01T00:00", "2012-04-01T00:00", "P1Q", "2012-Q1")
    assertTimeSpan(
      FindLater(Present, Map(QUARTER_OF_YEAR -> 2)),
      "2013-04-01T00:00", "2013-07-01T00:00", "P1Q", "2013-Q2")
    assertTimeSpan(
      FindLater(Present, Map(DECADE_OF_CENTURY -> 3)),
      "2030-01-01T00:00", "2040-01-01T00:00", "P1DE", "203")
    assertTimeSpan(
      FindEnclosing(Present, DECADES),
      "2010-01-01T00:00", "2020-01-01T00:00", "P1DE", "201")
    
    // this previously caused an infinite loop because searching one night at a time
    // managed to skip past Sunday night; so the test here is just that it completes
    val mar28 = TimeSpan.of(2000, 3, 28, 0, 0, 0)
    val mar26ni = FindEarlier(Present, Map(DAY_OF_WEEK -> 7, NIGHT_OF_DAY -> 1)).toTimeSpan(mar28)
    assert(mar26ni.timeMLValueOption === Some("2000-03-26TNI"))
    
    // this previously failed, accepting as Wednesday night the night that starts on Tuesday
    // (the problem was not checking that fields still match after truncation)
    val feb16 = TimeSpan.of(2000, 2, 16, 0, 0, 0)
    val feb16ni = FindAtOrEarlier(Present, Map(DAY_OF_WEEK -> 3, NIGHT_OF_DAY -> 1)).toTimeSpan(feb16)
    assert(feb16ni.timeMLValueOption === Some("2000-02-09TNI"))
    
    // these previously failed during the transition to TimeSpan-based anchors, because the
    // definition of "earlier" and "later" in FindEarlier, etc. was too restrictive
    val jan20 = TimeSpan.of(1998, 1, 20)
    val jan19ni = FindEarlier(Present, Map(NIGHT_OF_DAY -> 1)).toTimeSpan(jan20)
    assert(jan19ni.timeMLValueOption === Some("1998-01-19TNI"))
    
    val dec03 = TimeSpan.of(1991, 12, 3)
    val dec03again = FindAtOrEarlier(Present, Map(DAY_OF_WEEK -> 2)).toTimeSpan(dec03)
    assert(dec03again.timeMLValueOption === Some("1991-12-03"))
    val nov27 = FindAtOrEarlier(Present, Map(DAY_OF_WEEK -> 3)).toTimeSpan(dec03)
    assert(nov27.timeMLValueOption === Some("1991-11-27"))
    
    val oct08 = TimeSpan.of(1999, 10, 8)
    val oct08noon = FindAtOrEarlier(Present, Map(HOUR_OF_DAY -> 12, MINUTE_OF_HOUR -> 0)).toTimeSpan(oct08)
    assert(oct08noon.timeMLValueOption === Some("1999-10-08T12:00"))

    val earlierNight = FindEarlier(Present, Map(NIGHT_OF_DAY -> 1))
    val atOrEarlierNight = FindAtOrEarlier(Present, Map(NIGHT_OF_DAY -> 1))
    val atOrLaterNight = FindAtOrLater(Present, Map(NIGHT_OF_DAY -> 1))
    val laterNight = FindLater(Present, Map(NIGHT_OF_DAY -> 1))
    val feb15 = TimeSpan.of(2000, 2, 15)
    assert(earlierNight.toTimeSpan(feb15).timeMLValueOption === Some("2000-02-14TNI"))
    assert(atOrEarlierNight.toTimeSpan(feb15).timeMLValueOption === Some("2000-02-14TNI"))
    assert(atOrLaterNight.toTimeSpan(feb15).timeMLValueOption === Some("2000-02-15TNI"))
    assert(laterNight.toTimeSpan(feb15).timeMLValueOption === Some("2000-02-15TNI"))
    
    val laterTuesdayNight = FindLater(Present, Map(DAY_OF_WEEK -> 2, NIGHT_OF_DAY -> 1))
    assert(laterTuesdayNight.toTimeSpan(feb15).timeMLValueOption === Some("2000-02-15TNI"))
  }

  test("resolves complex time spans") {
    import PeriodParse.{Simple => SimplePeriod, Unspecified => UnspecifiedPeriod}
    import TimeSpanParse._
    assertTimeSpan(
      MoveLater(WithModifier(Present, Modifier.Approx), SimplePeriod(1, DAYS)),
      "2012-12-13T12:12:12", "2012-12-13T12:12:13", "PT1S", "2012-12-13T12:12:12", "APPROX")
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, DAYS), SimplePeriod(1, DAYS)),
      "2012-12-11T00:00", "2012-12-12T00:00", "P1D", "2012-12-11")
    assertTimeSpan(
      EndAtStartOf(FindEnclosing(Present, DAYS), SimplePeriod(1, DAYS)),
      "2012-12-11T00:00", "2012-12-12T00:00", "P1D", "2012-12-11")
    assertTimeSpan(
      MoveLater(FindEnclosing(Present, DAYS), SimplePeriod(1, WEEKS)),
      "2012-12-19T00:00", "2012-12-20T00:00", "P1D", "2012-12-19")
    assertTimeSpan(
      MoveLater(FindEnclosing(Present, WEEKS), SimplePeriod(1, WEEKS)),
      "2012-12-17T00:00", "2012-12-24T00:00", "P1W", "2012-W51")
    assertTimeSpan(
      StartAtEndOf(Present, SimplePeriod(1, WEEKS)),
      "2012-12-12T12:12:13", "2012-12-19T12:12:13", "P1W", null)
    assertTimeSpan(
      StartAtEndOf(FindEnclosing(Present, DAYS), SimplePeriod(1, WEEKS)),
      "2012-12-13T00:00", "2012-12-20T00:00", "P1W", null)
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, WEEKS), SimplePeriod(1, WEEKS)),
      "2012-12-03T00:00", "2012-12-10T00:00", "P1W", "2012-W49")
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, DAYS), SimplePeriod(1, WEEKS)),
      "2012-12-05T00:00", "2012-12-06T00:00", "P1D", "2012-12-05")
    assertTimeSpan(
      MoveEarlier(WithModifier(Present, Modifier.Approx), SimplePeriod(1, WEEKS)),
      "2012-12-05T12:12:12", "2012-12-05T12:12:13", "PT1S", "2012-12-05T12:12:12", "APPROX")
    assertTimeSpan(
      MoveLater(FindEnclosing(Present, MONTHS), SimplePeriod(1, MONTHS)),
      "2013-01-01T00:00", "2013-02-01T00:00", "P1M", "2013-01")
    assertTimeSpan(
      StartAtEndOf(FindEnclosing(Present, MONTHS), SimplePeriod(1, MONTHS)),
      "2013-01-01T00:00", "2013-02-01T00:00", "P1M", "2013-01")
    assertTimeSpan(
      MoveLater(FindEnclosing(Present, MONTHS), SimplePeriod(2, MONTHS)),
      "2013-02-01T00:00", "2013-03-01T00:00", "P1M", "2013-02")
    assertTimeSpan(
      MoveLater(FindEnclosing(Present, DAYS), SimplePeriod(1, MONTHS)),
      "2013-01-12T00:00", "2013-01-13T00:00", "P1D", "2013-01-12")
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, WEEKS), SimplePeriod(2, WEEKS)),
      "2012-11-26T00:00", "2012-12-03T00:00", "P1W", "2012-W48")
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, DAYS), SimplePeriod(2, WEEKS)),
      "2012-11-28T00:00", "2012-11-29T00:00", "P1D", "2012-11-28")
    assertTimeSpan(
      MoveEarlier(MoveEarlier(FindEnclosing(Present, DAYS), SimplePeriod(1, DAYS)), SimplePeriod(1, DAYS)),
      "2012-12-10T00:00", "2012-12-11T00:00", "P1D", "2012-12-10")
    assertTimeSpan(
      MoveLater(Present, SimplePeriod(2, DAYS)),
      "2012-12-14T12:12:12", "2012-12-14T12:12:13", "PT1S", "2012-12-14T12:12:12")
    assertTimeSpan(
      MoveLater(FindEnclosing(Present, DAYS), SimplePeriod(2, DAYS)),
      "2012-12-14T00:00", "2012-12-15T00:00", "P1D", "2012-12-14")
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, DAYS), SimplePeriod(1, DECADES)),
      "2002-12-12T00:00", "2002-12-13T00:00", "P1D", "2002-12-12")
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, DECADES), SimplePeriod(1, DECADES)),
      "2000-01-01T00:00", "2010-01-01T00:00", "P1DE", "200")
    assertTimeSpan(
      FindEnclosing(Present, CENTURIES),
      "2000-01-01T00:00", "2100-01-01T00:00", "P1CE", "20")
    assertTimeSpan(
      StartAtStartOf(FindAbsolute(Map(YEAR -> 2012)), SimplePeriod(1, DAYS)),
      "2012-01-01T00:00", "2012-01-02T00:00", "P1D", "2012-01-01")
    assertTimeSpan(
      FindEarlier(FindAbsolute(Map(YEAR -> 1998)), Map(MONTH_OF_YEAR -> 12)),
      "1997-12-01T00:00", "1998-01-01T00:00", "P1M", "1997-12")
    assertTimeSpan(
      FindLater(MoveLater(Present, SimplePeriod(2, DAYS)), Map(DAY_OF_MONTH -> 14)),
      "2013-01-14T00:00", "2013-01-15T00:00", "P1D", "2013-01-14")
    assertTimeSpan(
      FindEnclosed(FindEnclosing(Present, DAYS), Map(MORNING_OF_DAY -> 1)),
      "2012-12-12T00:00", "2012-12-12T12:00", "P1MO", "2012-12-12TMO")
    assertTimeSpan(
      FindEnclosed(FindEnclosing(Present, DAYS), Map(AFTERNOON_OF_DAY -> 1)),
      "2012-12-12T12:00", "2012-12-12T18:00", "P1AF", "2012-12-12TAF")
    assertTimeSpan(
      FindEnclosed(FindEnclosing(Present, DAYS), Map(EVENING_OF_DAY -> 1)),
      "2012-12-12T17:00", "2012-12-13T00:00", "P1EV", "2012-12-12TEV")
    assertTimeSpan(
      FindEnclosed(FindEnclosing(Present, DAYS), Map(NIGHT_OF_DAY -> 1)),
      "2012-12-12T21:00", "2012-12-13T04:00", "P1NI", "2012-12-12TNI")

    // moving with underspecified periods
    assertTimeSpan(
      MoveEarlier(Present, UnspecifiedPeriod(DAYS)),
      "-999999999-01-01T00:00", nowEnd, "PXD", null, "APPROX")
    assertTimeSpan(
      EndAtStartOf(Present, UnspecifiedPeriod(WEEKS)),
      "-999999999-01-01T00:00", nowStart, "PXW", null, "APPROX")
    assertTimeSpan(
      MoveLater(Present, UnspecifiedPeriod(MONTHS)),
      nowStart, "+999999999-12-31T23:59:59.999999999", "PXM", null, "APPROX")
    assertTimeSpan(
      StartAtEndOf(Present, UnspecifiedPeriod(YEARS)),
      nowEnd, "+999999999-12-31T23:59:59.999999999", "PXY", null, "APPROX")

    // this previously failed because we were using aligned weeks instead of Monday-aligned weeks
    val mar6 = TimeSpan.of(1998, 3, 6)
    val nextWeek = StartAtEndOf(FindEnclosing(Present, WEEKS), SimplePeriod(1, WEEKS))
    assert(nextWeek.toTimeSpan(mar6).timeMLValueOption === Some("1998-W11"))
    
    // this previously failed during the transition to TimeSpan-based anchors
    val nov2 = TimeSpan.of(1989, 11, 2)
    val quarter3 = EndAtStartOf(FindEnclosing(Present, QUARTER_YEARS), SimplePeriod(1, QUARTER_YEARS))
    assert(quarter3.toTimeSpan(nov2).timeMLValueOption === Some("1989-Q3"))
 }
  
  private def assertTimeSpan(
    timeSpanParse: TimeSpanParse,
    start: String,
    end: String,
    periodTimeMLValue: String,
    timeMLValue: String,
    timeMLModifier: String = null) = {
    val result = timeSpanParse.toTimeSpan(now)
    assert(result.start.toLocalDateTime.toString === start)
    assert(result.end.toLocalDateTime.toString === end) 
    assert(result.period.timeMLValue === periodTimeMLValue)
    assert(result.timeMLValueOption === Option(timeMLValue))
    assert(result.modifier.timeMLValueOption === Option(timeMLModifier))
  }

  test("resolves time span sets") {
    import TimeSpanSetParse._
    assertTimeSpanSet(Simple(Map(MONTH_OF_YEAR -> 3)), "XXXX-03")
    assertTimeSpanSet(Simple(Map(YEAR -> 1998, DAY_OF_MONTH -> 22)), "1998-XX-22")
    assertTimeSpanSet(Simple(Map(DAY_OF_WEEK -> 1)), "XXXX-WXX-1")
    assertTimeSpanSet(Simple(Map(DAY_OF_WEEK -> 2, NIGHT_OF_DAY -> 1)), "XXXX-WXX-2TNI")
    assertTimeSpanSet(Simple(Map(MINUTE_OF_HOUR -> 15)), "XXXX-XX-XXTXX:15")
    assertTimeSpanSet(Simple(Map(ISO_WEEK.OF_YEAR -> 12, MINUTE_OF_HOUR -> 15)), "XXXX-W12TXX:15")
    assertTimeSpanSet(Simple(Map(QUARTER_OF_YEAR -> 3)), "XXXX-Q3")
  }

  private def assertTimeSpanSet(
    timeSpanSetParse: TimeSpanSetParse,
    timeMLValue: String) = {
    val result = timeSpanSetParse.toTimeSpanSet
    assert(result.timeMLValue === timeMLValue)
  }

}
