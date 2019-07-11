package org.clulab.timenorm.neural

import java.time.temporal.ChronoField

import org.clulab.timenorm.formal.{Interval, RepeatingField, SimpleInterval, TypesSuite}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TemporalNeuralParserTest extends FunSuite with TypesSuite {

  private val parser = new TemporalNeuralParser()
  private val Array(dct: Interval) = parser.parse("2018-07-06")
  private val batch = parser.parseBatch(
    """
      |2018-10-10
      |January
      |last Friday
      |South Sudan receives a ranking of 186 out of 189 on
      |ease of doing business in the World Bank 2015 Doing
      |Business report -LRB- World Bank 2014 -RRB- .
      |since last March
      |A substantial decline in gas revenue since 2014 has
      |contributed to a sharp drop in both foreign currency
      |reserves and the value of the South Sudanese pound.
    """.stripMargin.trim,
    Array(
      (0, 10),    // 2018-10-10
      (11, 18),   // January
      (19, 30),   // last Friday
      (31, 180),  // South Sudan ... -RRB- .
      (181, 197), // since last March
      (198, 354), // A substantial ... pound.
    ),
    // use a SimpleInterval here so that there's no associated character span
    SimpleInterval(dct.start, dct.end),
  )


  test("interval") {
    val Array(year: Interval) = batch(0)
    assert(year.charSpan === Some((0, 10)))
    assert(year === SimpleInterval.of(2018, 10, 10))
  }

  test("repeatingInterval") {
    val Array(january: RepeatingField) = batch(1)
    assert(january.charSpan === Some((11, 18)))
    assert(january.field === ChronoField.MONTH_OF_YEAR)
    assert(january.value === 1)
  }

  test("last") {
    val Array(friday: Interval) = batch(2)
    assert(friday.charSpan === Some((19, 30)))
    assert(friday === SimpleInterval.of(2018, 6, 29))
  }

  test("fill-incomplete-span") {
    // 2014 is not found (probably because of the -RRB-), so don't test for that
    val Some(year2015: Interval) = batch(3).headOption
    assert(year2015.charSpan === Some((124, 128)))
    assert(year2015 === SimpleInterval.of(2015))
  }

  test("since-last") {
    val Array(march: Interval) = batch(4)
    assert(march.charSpan === Some((181, 197)))
    assert(march.start === SimpleInterval.of(2018, 3, 1).start)
    assert(march.end === dct.end)
  }

  test("since-year") {
    val Array(since2014: Interval) = batch(5)
    assert(since2014.charSpan === Some((235, 245)))
    assert(since2014.start === SimpleInterval.of(2014).start)
    assert(since2014.end === dct.end)
  }
}
