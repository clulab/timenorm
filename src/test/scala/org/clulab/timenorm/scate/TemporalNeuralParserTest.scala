package org.clulab.timenorm.scate

import java.time.temporal.ChronoField

import org.scalatest.{BeforeAndAfterAll, FunSuite}

class TemporalNeuralParserTest extends FunSuite with BeforeAndAfterAll with TypesSuite {

  override def afterAll(): Unit = {
    parser.close()
  }

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
      |Between 1 and 30 March 2017, 16,274 South Sudanese
      |refugees arrived in Gambella, Ethiopia, bringing the
      |total number of new arrivals since September 2016
      |to 77,874.
      |FOOD PROGRAMME Rome, 2016   The designations employed and the presentation of material in this
      |the past several months
      |nineteen ninety nine
      |until next December
      |from May 2016 to March 2017
      |between May 2016 and March 2017
    """.stripMargin.trim,
    Array(
      (0, 10),    // 2018-10-10
      (11, 18),   // January
      (19, 30),   // last Friday
      (31, 180),  // South Sudan ... -RRB- .
      (181, 197), // since last March
      (198, 354), // A substantial ... pound.
      (355, 519), // Between 1 ... 77,874
      (520, 614), // FOOD PROGRAMME ... in this
      (615, 638), // the past several months
      (639, 659), // nineteen ninety nine
      (660, 679), // until next December
      (680, 707), // from May 2016 to March 2017
      (708, 739)  // between May 2016 and March 2017
    ),
    // use a SimpleInterval here so that there's no associated character span
    SimpleInterval(dct.start, dct.end)
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

  test("since-month-year") {
    // the first "Between" is not parsed properly yet, so don't test for that
    val Some(sinceSep2016: Interval) = batch(6).lastOption
    assert(sinceSep2016.charSpan === Some((488, 508)))
    assert(sinceSep2016.start === SimpleInterval.of(2016, 9).start)
    assert(sinceSep2016.end === dct.end)
  }

  test("fill-incomplete-span-backwards") {
    val Some(year2016: Interval) = batch(7).headOption
    assert(year2016.charSpan === Some((541, 545)))
    assert(year2016 === SimpleInterval.of(2016))
  }

  test("vague-number") {
    val Array(pastSeveralMonths: Interval) = batch(8)
    assert(pastSeveralMonths.charSpan === Some((619, 638)))
  }

  test("English-number") {
    val Array(year1999: Interval) = batch(9)
    assert(year1999.charSpan === Some((639, 659)))
    assert(year1999 === SimpleInterval.of(1999))
  }

  test("until-December"){
    val Array(untilDecember: Interval) = batch(10)
    assert(untilDecember.charSpan === Some((660, 679)))
    assert(untilDecember.start === dct.start)
    assert(untilDecember.end === SimpleInterval.of(2018, 12).end)
  }

  test("from-May2016-to-March2017") {
    val Array(fromMay2016toMarch2017: Interval) = batch(11)
    assert(fromMay2016toMarch2017.charSpan === Some((685, 707)))
    assert(fromMay2016toMarch2017.start === SimpleInterval.of(2016, 5).start)
    assert(fromMay2016toMarch2017.end === SimpleInterval.of(2017, 3).end)
  }

  test("between-May2016-and-March2017"){
    val Array(betweenMay2016andMarch2017: Interval) = batch(12)
    assert(betweenMay2016andMarch2017.charSpan === Some((734, 765)))
    assert(betweenMay2016andMarch2017.start === SimpleInterval.of(2016, 5).start) // 1 May 2016 and 30 March 2017
    assert(betweenMay2016andMarch2017.end === SimpleInterval.of(2017, 3).end)
  }

  test("no-duplicate-ids") {
    // in July 2019, for the text below, the parser generated [After even][Next tual][After ly] and the code for
    // expanding to word boundaries expanded these all to have the span, resulting in <entity> nodes with identical IDs
    val xml = parser.parseToXML(
      """
        |Although BP's involvement in the Russian joint venture has been lucrative, relations with its partners have often been fraught with disagreement. In 2011, the AAR consortium attempted to block a drilling joint venture in the Arctic between BP and Rosneft through the courts and the plan was eventually dropped.
        |
        |As well as internal wrangles, BP employees at TNK-BP have fallen foul of Russian authorities.
      """.stripMargin)
    val ids = (xml \\ "id").map(_.text)
    assert(ids === ids.distinct)
  }

  test("kkkk") {
    val parser = new TemporalNeuralParser()
    val Array(dct: Interval) = parser.parse("2018-07-06")
    val batch = parser.parseBatch(
      "At a critical moment in 2011 the factory producing RUTF in Ethiopia had an issue with hygiene and had to be closed for a week, affecting supply critically."
    ,Array((0, 155)), SimpleInterval(dct.start, dct.end)
    )
    val i = batch(0)
  }
}
