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
    import Temporal.Period._
    assert(SimplePeriod(2, WEEKS).toTimeMLValue === "P2W")
    assert(SimplePeriod(10, DAYS).toTimeMLValue === "P10D")
    assert(SimplePeriod(1, MONTHS).toTimeMLValue === "P1M")
    assert(SimplePeriod(16, HOURS).toTimeMLValue === "PT16H")
    assert(SimplePeriod(20, MINUTES).toTimeMLValue === "PT20M")
    assert(SimplePeriod(53, SECONDS).toTimeMLValue === "PT53S")
  }

  test("resolves complex periods") {
    import Temporal.Period._
    assert(Plus(SimplePeriod(2, WEEKS), SimplePeriod(1, DAYS)).toTimeMLValue === "P2W1D")
  }

  val now = ZonedDateTime.of(LocalDateTime.of(2012, 12, 12, 12, 12, 12), ZoneId.of("-12:00"))

  test("resolves simple anchors") {
    import Temporal.Anchor._
    assertAnchor(
      Today,
      DAYS, DAYS, "2012-12-12", "2012-12-12")
    assertAnchor(
      Now,
      SECONDS, SECONDS, "PRESENT_REF", "PRESENT_REF")
    assertAnchor(
      Date(1976, 9, 21),
      DAYS, DAYS, "1976-09-21", "1976-09-21")
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
  }

  test("resolves complex anchors") {
    import Temporal.Period.SimplePeriod
    import Temporal.Anchor._
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
      Plus(Now, SimplePeriod(2, DAYS)),
      SECONDS, DAYS, "2012-12-14T12:12:12", "2012-12-14")
  }

  private def assertAnchor(
    anchor: Temporal.Anchor,
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
