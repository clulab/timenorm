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
    assertPeriod(SimplePeriod(2, WEEKS), "P2W", WEEKS -> 2)
    assertPeriod(SimplePeriod(10, DAYS), "P10D", DAYS -> 10)
    assertPeriod(SimplePeriod(1, MONTHS), "P1M", MONTHS -> 1)
    assertPeriod(SimplePeriod(16, HOURS), "PT16H", HOURS -> 16)
    assertPeriod(SimplePeriod(20, MINUTES), "PT20M", MINUTES -> 20)
    assertPeriod(SimplePeriod(53, SECONDS), "PT53S", SECONDS -> 53)
  }

  test("resolves complex periods") {
    import PeriodParse._
    assertPeriod(
        Plus(SimplePeriod(2, WEEKS), SimplePeriod(1, DAYS)),
        "P2W1D", WEEKS -> 2, DAYS -> 1)
    assertPeriod(
        Plus(SimplePeriod(2, DAYS), SimplePeriod(1, DAYS)),
        "P3D", DAYS -> 3)
    assertPeriod(
        Minus(SimplePeriod(5, MONTHS), Minus(SimplePeriod(3, MONTHS), SimplePeriod(1, MONTHS))),
        "P3M", MONTHS -> 3)
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

  test("resolves simple anchors") {
    import AnchorParse._
    assertAnchor(
      Today,
      DAYS, DAYS, "2012-12-12", "2012-12-12")
    assertAnchor(
      Past,
      FOREVER, FOREVER, "PAST_REF", "PAST_REF")
    assertAnchor(
      Present,
      SECONDS, FOREVER, "PRESENT_REF", "PRESENT_REF")
    assertAnchor(
      Future,
      FOREVER, FOREVER, "FUTURE_REF", "FUTURE_REF")
    assertAnchor(
      Date(Map(YEAR -> 1976, MONTH_OF_YEAR -> 9, DAY_OF_MONTH -> 21)),
      DAYS, DAYS, "1976-09-21", "1976-09-21")
    assertAnchor(
      Date(Map(YEAR -> 2030)),
      YEARS, YEARS, "2030", "2030")
    assertAnchor(
      Previous(Map(MONTH_OF_YEAR -> 10, DAY_OF_MONTH -> 15)),
      DAYS, DAYS, "2012-10-15", "2012-10-15")
    assertAnchor(
      Next(Map(MONTH_OF_YEAR -> 2)),
      MONTHS, MONTHS, "2013-02", "2013-02")
    assertAnchor(
      Next(Map(DAY_OF_WEEK -> DayOfWeek.FRIDAY.getValue, DAY_OF_MONTH -> 13)),
      DAYS, DAYS, "2013-09-13", "2013-09-13")
    assertAnchor(
      Closest(Map(DAY_OF_WEEK -> DayOfWeek.TUESDAY.getValue)),
      DAYS, DAYS, "2012-12-11", "2012-12-11")
    assertAnchor(
      Closest(Map(DAY_OF_WEEK -> DayOfWeek.WEDNESDAY.getValue)),
      DAYS, DAYS, "2012-12-12", "2012-12-12")
    assertAnchor(
      Closest(Map(DAY_OF_WEEK -> DayOfWeek.THURSDAY.getValue)),
      DAYS, DAYS, "2012-12-13", "2012-12-13")
    assertAnchor(
      Previous(Map(CLOCK_HOUR_OF_AMPM -> 11, MINUTE_OF_HOUR -> 18, AMPM_OF_DAY -> 0)),
      MINUTES, MINUTES, "2012-12-12T11:18", "2012-12-12T11:18")
    assertAnchor(
      CurrentOrPrevious(Map(DAY_OF_WEEK -> DayOfWeek.WEDNESDAY.getValue)),
      DAYS, DAYS, "2012-12-12", "2012-12-12")
    assertAnchor(
      CurrentOrPrevious(Map(DAY_OF_WEEK -> DayOfWeek.TUESDAY.getValue)),
      DAYS, DAYS, "2012-12-11", "2012-12-11")
    assertAnchor(
      Previous(Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      DAYS, DAYS, "2011-12-12", "2011-12-12")
    assertAnchor(
      CurrentOrPrevious(Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      DAYS, DAYS, "2012-12-12", "2012-12-12")
    assertAnchor(
      Next(Map(MONTH_OF_YEAR -> 12, DAY_OF_MONTH -> 12)),
      DAYS, DAYS, "2013-12-12", "2013-12-12")
    assertAnchor(
      MinUnit(Today, WEEKS),
      WEEKS, WEEKS, "2012-W50", "2012-W50")
  }

  test("resolves complex anchors") {
    import PeriodParse.SimplePeriod
    import AnchorParse._
    assertAnchor(
      Plus(Today, SimplePeriod(1, DAYS)),
      DAYS, DAYS, "2012-12-13", "2012-12-13")
    assertAnchor(
      Minus(Today, SimplePeriod(1, DAYS)),
      DAYS, DAYS, "2012-12-11", "2012-12-11")
    assertAnchor(
      Plus(Today, SimplePeriod(1, WEEKS)),
      DAYS, WEEKS, "2012-12-19", "2012-W51")
    assertAnchor(
      Minus(Today, SimplePeriod(1, WEEKS)),
      DAYS, WEEKS, "2012-12-05", "2012-W49")
    assertAnchor(
      Plus(Today, SimplePeriod(1, MONTHS)),
      DAYS, MONTHS, "2013-01-12", "2013-01")
    assertAnchor(
      Minus(Today, SimplePeriod(2, WEEKS)),
      DAYS, WEEKS, "2012-11-28", "2012-W48")
    assertAnchor(
      Minus(Minus(Today, SimplePeriod(1, DAYS)), SimplePeriod(1, DAYS)),
      DAYS, DAYS, "2012-12-10", "2012-12-10")
    assertAnchor(
      Plus(Present, SimplePeriod(2, DAYS)),
      SECONDS, DAYS, "2012-12-14T12:12:12", "2012-12-14")
  }

  private def assertAnchor(
    anchor: AnchorParse,
    baseUnit: ChronoUnit,
    rangeUnit: ChronoUnit,
    baseTimeMLValue: String,
    rangeTimeMLValue: String) = {
    val result = anchor.toDateTime(now)
    assert(result.baseUnit === baseUnit)
    assert(result.rangeUnit === rangeUnit)
    assert(result.baseTimeMLValue === baseTimeMLValue)
    assert(result.rangeTimeMLValue === rangeTimeMLValue)
  }
}
