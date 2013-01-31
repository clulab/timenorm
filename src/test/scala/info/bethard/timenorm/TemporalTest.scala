package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.ZoneId

@RunWith(classOf[JUnitRunner])
class TemporalTest extends FunSuite {

  test("resolves simple periods") {
    assert(Temporal.Period.SimplePeriod(2, ChronoUnit.WEEKS).toTimeMLValue === "P2W")
    assert(Temporal.Period.SimplePeriod(10, ChronoUnit.DAYS).toTimeMLValue === "P10D")
    assert(Temporal.Period.SimplePeriod(1, ChronoUnit.MONTHS).toTimeMLValue === "P1M")
  }

  test("resolves complex periods") {
    assert(
      Temporal.Period.Plus(
        Temporal.Period.SimplePeriod(2, ChronoUnit.WEEKS),
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)).toTimeMLValue === "P2W1D")
  }
  
  val now = ZonedDateTime.of(LocalDateTime.of(2012, 12, 12, 12, 12, 12), ZoneId.of("-12:00"))
  
  test("resolves simple anchors") {
    assert(Temporal.Anchor.Today.toTimeMLValue(now) === "2012-12-12")
    assert(Temporal.Anchor.Of(Map(
        ChronoField.MONTH_OF_YEAR -> 9,
        ChronoField.DAY_OF_MONTH -> 21,
        ChronoField.YEAR -> 1976)).toTimeMLValue(now) === "1976-09-21")
    assert(Temporal.Anchor.Of(Map(
        ChronoField.MONTH_OF_YEAR -> 10,
        ChronoField.DAY_OF_MONTH -> 15)).toTimeMLValue(now) == "2012-10-15")
  }
}
