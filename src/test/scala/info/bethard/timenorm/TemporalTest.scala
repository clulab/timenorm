package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoField

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
}
