package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.DayOfWeek

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

  val now = ZonedDateTime.of(LocalDateTime.of(2012, 12, 12, 12, 12, 12), ZoneId.of("-12:00"))
  val nowString = now.getDateTime.toString

  test("resolves simple time spans") {
    import TimeSpanParse._
    assertTimeSpan(
      Past,
      "-999999999-01-01T00:00", nowString, "PX", "PAST_REF", "APPROX")
    assertTimeSpan(
      Present,
      nowString, nowString, "P", "PRESENT_REF")
    assertTimeSpan(
      Future,
      nowString, "+999999999-12-31T23:59:59.999999999", "PX", "FUTURE_REF", "APPROX")
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
      FindEarlier(Map(SPRING_OF_YEAR -> 1)),
      "2012-03-20T00:00", "2012-06-21T00:00", "P1SP", "2012-SP")
    assertTimeSpan(
      FindLater(Map(SUMMER_OF_YEAR -> 1)),
      "2013-06-21T00:00", "2013-09-22T00:00", "P1SU", "2013-SU")
    assertTimeSpan(
      FindEarlier(Map(FALL_OF_YEAR -> 1)),
      "2011-09-22T00:00", "2011-12-21T00:00", "P1FA", "2011-FA")
    assertTimeSpan(
      FindLater(Map(WINTER_OF_YEAR -> 1)),
      "2012-12-21T00:00", "2013-03-20T00:00", "P1WI", "2012-WI")
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
      FindEarlier(Map(MORNING_OF_DAY -> 1)),
      "2012-12-12T00:00", "2012-12-12T12:00", "P1MO", "2012-12-12TMO")
    assertTimeSpan(
      FindLater(Map(AFTERNOON_OF_DAY -> 1)),
      "2012-12-13T12:00", "2012-12-13T18:00", "P1AF", "2012-12-13TAF")
    assertTimeSpan(
      FindEarlier(Map(EVENING_OF_DAY -> 1)),
      "2012-12-11T17:00", "2012-12-12T00:00", "P1EV", "2012-12-11TEV")
    assertTimeSpan(
      FindLater(Map(NIGHT_OF_DAY -> 1)),
      "2012-12-12T21:00", "2012-12-13T04:00", "P1NI", "2012-12-12TNI")
    assertTimeSpan(
      FindEarlier(Map(NIGHT_OF_DAY -> 1)),
      "2012-12-11T21:00", "2012-12-12T04:00", "P1NI", "2012-12-11TNI")
    assertTimeSpan(
      FindEnclosing(Present, AFTERNOONS),
      "2012-12-12T12:00", "2012-12-12T18:00", "P1AF", "2012-12-12TAF")
    assertTimeSpan(
      FindLater(Map(WEEKEND_OF_WEEK -> 1)),
      "2012-12-15T00:00", "2012-12-17T00:00", "P1WE", "2012-W50-WE")
    assertTimeSpan(
      FindLater(Map(EASTER_DAY_OF_YEAR -> 1)),
      "2013-03-31T00:00", "2013-04-01T00:00", "P1D", "2013-03-31")
    assertTimeSpan(
      FindEarlier(Map(EASTER_DAY_OF_YEAR -> 1)),
      "2012-04-08T00:00", "2012-04-09T00:00", "P1D", "2012-04-08")
    
    // this previously caused an infinite loop because searching one night at a time
    // managed to skip past Sunday night; so the test here is just that it completes
    val mar28 = ZonedDateTime.of(LocalDateTime.of(2000, 3, 28, 0, 0), ZoneId.of("Z"))
    val mar26ni = FindEarlier(Map(DAY_OF_WEEK -> 7, NIGHT_OF_DAY -> 1)).toTimeSpan(mar28)
    assert(mar26ni.timeMLValueOption === Some("2000-03-26TNI"))
    
    // this previously failed, accepting as Wednesday night the night that starts on Tuesday
    // (the problem was not checking that fields still match after truncation)
    val feb16 = ZonedDateTime.of(LocalDateTime.of(2000, 2, 16, 0, 0), ZoneId.of("Z"))
    val feb16ni = FindCurrentOrEarlier(Map(DAY_OF_WEEK -> 3, NIGHT_OF_DAY -> 1)).toTimeSpan(feb16)
    assert(feb16ni.timeMLValueOption === Some("2000-02-09TNI"))
  }

  test("resolves complex time spans") {
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
      "2000-01-01T00:00", "2010-01-01T00:00", "P1DE", "200")
    assertTimeSpan(
      FindEnclosing(Present, CENTURIES),
      "2000-01-01T00:00", "2100-01-01T00:00", "P1CE", "20")
  }

  private def assertTimeSpan(
    timeSpanParse: TimeSpanParse,
    start: String,
    end: String,
    periodTimeMLValue: String,
    timeMLValue: String,
    timeMLModifier: String = null) = {
    val result = timeSpanParse.toTimeSpan(now)
    assert(result.start.getDateTime.toString === start)
    assert(result.end.getDateTime.toString === end) 
    assert(result.period.timeMLValue === periodTimeMLValue)
    assert(result.timeMLValueOption === Option(timeMLValue))
    assert(result.modifier.timeMLValueOption === Option(timeMLModifier))
  }
}