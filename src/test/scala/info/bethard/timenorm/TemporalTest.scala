package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.DayOfWeek

@RunWith(classOf[JUnitRunner])
class TemporalTest extends FunSuite {

  test("resolves simple periods") {
    import PeriodParse._
    assertPeriod(Simple(3, CENTURIES), "P300Y", CENTURIES -> 3)
    assertPeriod(Simple(2, WEEKS), "P2W", WEEKS -> 2)
    assertPeriod(Simple(10, DAYS), "P10D", DAYS -> 10)
    assertPeriod(Simple(1, MONTHS), "P1M", MONTHS -> 1)
    assertPeriod(Simple(16, HOURS), "PT16H", HOURS -> 16)
    assertPeriod(Simple(20, MINUTES), "PT20M", MINUTES -> 20)
    assertPeriod(Simple(53, SECONDS), "PT53S", SECONDS -> 53)
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
        Sum(Seq(Simple(191, YEARS), Simple(2, DECADES), Simple(3, CENTURIES))),
        "P511Y", YEARS -> 191, DECADES -> 2, CENTURIES -> 3)
  }

  private def assertPeriod(
    periodParse: PeriodParse,
    timeMLValue: String,
    unitAmounts: (ChronoUnit, Int)*) = {
    val period = periodParse.toPeriod
    assert(period.timeMLValue === timeMLValue)
    assert(period.unitAmounts === unitAmounts.toMap)
  }

  val now = ZonedDateTime.of(LocalDateTime.of(2012, 12, 12, 12, 12, 12), ZoneId.of("-12:00"))
  val nowString = now.getDateTime.toString

  test("resolves simple anchors") {
    import TimeSpanParse._
    assertTimeSpan(
      Past,
      null, nowString, null, "PAST_REF", "APPROX")
    assertTimeSpan(
      Present,
      nowString, nowString, "P", "PRESENT_REF")
    assertTimeSpan(
      Future,
      nowString, null, null, "FUTURE_REF", "APPROX")
    assertTimeSpan(
      FindAbsolute(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)),
      "1976-09-21T00:00", "1976-09-22T00:00", "P1D", "1976-09-21")
    assertTimeSpan(
      FindAbsolute(Map(YEAR -> 2030)),
      "2030-01-01T00:00", "2031-01-01T00:00", "P1Y", "2030")
    assertTimeSpan(
      FindEarlier(Map(MONTH_OF_YEAR -> 10, DAY_OF_MONTH -> 15)),
      "2012-10-15T00:00", "2012-10-16T00:00", "P1D", "2012-10-15")
    assertTimeSpan(
      FindLater(Map(MONTH_OF_YEAR -> 2)),
      "2013-02-01T00:00", "2013-03-01T00:00", "P1M", "2013-02")
    assertTimeSpan(
      FindLater(Map(DAY_OF_WEEK -> DayOfWeek.FRIDAY.getValue, DAY_OF_MONTH -> 13)),
      "2013-09-13T00:00", "2013-09-14T00:00", "P1D", "2013-09-13")
    assertTimeSpan(
      FindEarlier(Map(CLOCK_HOUR_OF_AMPM -> 11, MINUTE_OF_HOUR -> 18, AMPM_OF_DAY -> 0)),
      "2012-12-12T11:18", "2012-12-12T11:19", "PT1M", "2012-12-12T11:18")
    assertTimeSpan(
      FindEarlier(Map(SEASON_OF_YEAR -> 0)),
      "2012-03-20T00:00", "2012-06-21T00:00", "P1S", "2012-SP")
    assertTimeSpan(
      FindLater(Map(SEASON_OF_YEAR -> 3)),
      "2012-12-21T00:00", "2013-03-20T00:00", "P1S", "2012-WI")
    assertTimeSpan(
      FindCurrentOrEarlier(Map(DAY_OF_WEEK -> DayOfWeek.WEDNESDAY.getValue)),
      "2012-12-12T00:00", "2012-12-13T00:00", "P1D", "2012-12-12")
    assertTimeSpan(
      FindCurrentOrEarlier(Map(DAY_OF_WEEK -> DayOfWeek.TUESDAY.getValue)),
      "2012-12-11T00:00", "2012-12-12T00:00", "P1D", "2012-12-11")
    assertTimeSpan(
      FindEarlier(Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      "2011-12-12T00:00", "2011-12-13T00:00", "P1D", "2011-12-12")
    assertTimeSpan(
      FindCurrentOrEarlier(Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      "2012-12-12T00:00", "2012-12-13T00:00", "P1D", "2012-12-12")
    assertTimeSpan(
      FindLater(Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      "2013-12-12T00:00", "2013-12-13T00:00", "P1D", "2013-12-12")
    assertTimeSpan(
      FindEnclosing(Present, DAYS),
      "2012-12-12T00:00", "2012-12-13T00:00", "P1D", "2012-12-12")
    assertTimeSpan(
      FindEnclosing(Present, WEEKS),
      "2012-12-10T00:00", "2012-12-17T00:00", "P1W", "2012-W50")
    assertTimeSpan(
      FindLater(Map(QUARTER_OF_DAY -> 0)),
      "2012-12-13T00:00", "2012-12-13T06:00", "PT6H", "2012-12-13TNI")
    assertTimeSpan(
      FindEarlier(Map(QUARTER_OF_DAY -> 1)),
      "2012-12-12T06:00", "2012-12-12T12:00", "PT6H", "2012-12-12TMO")
    assertTimeSpan(
      FindLater(Map(QUARTER_OF_DAY -> 2)),
      "2012-12-13T12:00", "2012-12-13T18:00", "PT6H", "2012-12-13TAF")
    assertTimeSpan(
      FindLater(Map(QUARTER_OF_DAY -> 3)),
      "2012-12-12T18:00", "2012-12-13T00:00", "PT6H", "2012-12-12TEV")
  }

  test("resolves complex anchors") {
    import PeriodParse.{ Simple => SimplePeriod }
    import TimeSpanParse._
    assertTimeSpan(
      MoveLater(WithModifier(Present, Modifier.Approx), SimplePeriod(1, DAYS)),
      "2012-12-13T12:12:12", "2012-12-13T12:12:12", "P", "2012-12-13T12:12:12", "APPROX")
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
      "2012-12-12T12:12:12", "2012-12-19T12:12:12", "P1W", null)
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
      "2012-12-05T12:12:12", "2012-12-05T12:12:12", "P", "2012-12-05T12:12:12", "APPROX")
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
      "2012-12-14T12:12:12", "2012-12-14T12:12:12", "P", "2012-12-14T12:12:12")
    assertTimeSpan(
      MoveLater(FindEnclosing(Present, DAYS), SimplePeriod(2, DAYS)),
      "2012-12-14T00:00", "2012-12-15T00:00", "P1D", "2012-12-14")
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, DAYS), SimplePeriod(1, DECADES)),
      "2002-12-12T00:00", "2002-12-13T00:00", "P1D", "2002-12-12")
    assertTimeSpan(
      MoveEarlier(FindEnclosing(Present, DECADES), SimplePeriod(1, DECADES)),
      "2000-01-01T00:00", "2010-01-01T00:00", "P10Y", "200")
    assertTimeSpan(
      FindEnclosing(Present, CENTURIES),
      "2000-01-01T00:00", "2100-01-01T00:00", "P100Y", "20")
  }

  private def assertTimeSpan(
    timeSpanParse: TimeSpanParse,
    start: String,
    end: String,
    periodTimeMLValue: String,
    timeMLValue: String,
    timeMLModifier: String = null) = {
    val result = timeSpanParse.toTimeSpan(now)
    Option(start) match {
      case None => assert(result.start === null)
      case Some(start) => assert(result.start.getDateTime.toString === start)
    }
    Option(end) match {
      case None => assert(result.end === null)
      case Some(end) => assert(result.end.getDateTime.toString === end) 
    }
    Option(periodTimeMLValue) match {
      case None => assert(result.period === null)
      case Some(periodTimeMLValue) => assert(result.period.timeMLValue === periodTimeMLValue)
    }
    assert(result.timeMLValueOption === Option(timeMLValue))
    assert(result.modifier.timeMLValueOption === Option(timeMLModifier))
  }
}
