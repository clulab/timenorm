package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
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
  }

  test("resolves complex periods") {
    import Temporal.Period._
    assert(Plus(SimplePeriod(2, WEEKS), SimplePeriod(1, DAYS)).toTimeMLValue === "P2W1D")
  }

  val now = ZonedDateTime.of(LocalDateTime.of(2012, 12, 12, 12, 12, 12), ZoneId.of("-12:00"))

  test("resolves simple anchors") {
    import Temporal.Anchor._
    assert(Today.toTimeMLValue(now) === "2012-12-12")
    assert(Date(1976, 9, 21).toTimeMLValue(now) === "1976-09-21")
    assert(Previous(Map(MONTH_OF_YEAR -> 10, DAY_OF_MONTH -> 15)).toTimeMLValue(now) ===
      "2012-10-15")
    assert(Next(Map(MONTH_OF_YEAR -> 2)).toTimeMLValue(now) === "2013-02")
    val friday = DayOfWeek.FRIDAY.getValue()
    assert(Next(Map(DAY_OF_WEEK -> friday, DAY_OF_MONTH -> 13)).toTimeMLValue(now) ===
      "2013-09-13")
    assert(Closest(Map(DAY_OF_WEEK -> DayOfWeek.TUESDAY.getValue)).toTimeMLValue(now) ===
      "2012-12-11")
    assert(Closest(Map(DAY_OF_WEEK -> DayOfWeek.WEDNESDAY.getValue)).toTimeMLValue(now) ===
      "2012-12-12")
    assert(Closest(Map(DAY_OF_WEEK -> DayOfWeek.THURSDAY.getValue)).toTimeMLValue(now) ===
      "2012-12-13")
    assert(Previous(Map(CLOCK_HOUR_OF_AMPM -> 11, MINUTE_OF_HOUR -> 18, AMPM_OF_DAY -> 0)).toTimeMLValue(now) ===
      "2012-12-12T11:18")
  }

  test("resolves complex anchors") {
    import Temporal.Period.SimplePeriod
    import Temporal.Anchor._
    assert(Plus(Today, SimplePeriod(1, DAYS)).toTimeMLValue(now) === "2012-12-13")
    assert(Minus(Today, SimplePeriod(1, DAYS)).toTimeMLValue(now) === "2012-12-11")
    assert(Plus(Today, SimplePeriod(1, WEEKS)).toTimeMLValue(now) === "2012-12-19")
    assert(Minus(Today, SimplePeriod(1, WEEKS)).toTimeMLValue(now) === "2012-12-05")
    assert(Plus(Today, SimplePeriod(1, MONTHS)).toTimeMLValue(now) === "2013-01-12")
    assert(Minus(Today, SimplePeriod(2, WEEKS)).toTimeMLValue(now) === "2012-11-28")
    assert(Minus(Minus(Today, SimplePeriod(1, DAYS)), SimplePeriod(1, DAYS)).toTimeMLValue(now) ===
      "2012-12-10")
  }
}
