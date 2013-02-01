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
  }
}
