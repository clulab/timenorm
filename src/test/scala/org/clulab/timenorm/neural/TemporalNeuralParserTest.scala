package org.clulab.timenorm

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.LocalDateTime
import java.nio.file.Paths


@RunWith(classOf[JUnitRunner])
class TemporalCharbasedParserTest extends FunSuite {

  Thread.sleep(10000)

  val modelFile = this.getClass.getResource("/org/clulab/timenorm/model/char-3softmax-extra/weights-improvement-22.v2.dl4j.zip")
  val parser = new TemporalCharbasedParser(Paths.get(modelFile.toURI).toFile().getAbsolutePath)
  val anchor = parser.dct(parser.parse("2018-07-06"))

  test("parse-interval") {
    val date = "2018-10-10"
    val data = parser.parse(date)
    val intervals = parser.intervals(data, Some(anchor))

    assert(intervals(0)._1 == (0, 10))
    assert(intervals(0)._2(0)._1 === LocalDateTime.of(2018, 10, 10, 0, 0))
    assert(intervals(0)._2(0)._2 === LocalDateTime.of(2018, 10, 11, 0, 0))
    assert(intervals(0)._2(0)._3 === 86400)
  }

  test("parse-repeatingInterval") {
    val date = "January"
    val data = parser.parse(date)
    val intervals = parser.intervals(data, Some(anchor))

    assert(intervals(0)._1 == (0, 7))
    assert(intervals(0)._2(0)._1 === null)
    assert(intervals(0)._2(0)._2 === null)
    assert(intervals(0)._2(0)._3 === 2678400)
  }

  test("parse-last") {
    val date = "last Friday"
    val data = parser.parse(date)
    val intervals = parser.intervals(data, Some(anchor))

    assert(intervals(0)._1 == (0, 11))
    assert(intervals(0)._2(0)._1 === LocalDateTime.of(2018, 6, 29, 0, 0))
    assert(intervals(0)._2(0)._2 === LocalDateTime.of(2018, 6, 30, 0, 0))
    assert(intervals(0)._2(0)._3 === 86400)
  }

  test("fill-not-complete-span") {
    val date = "South Sudan receives a ranking of 186 out of 189 on ease of doing business in the World Bank 2015 Doing Business report -LRB- World Bank 2014 -RRB- ."
    val data = parser.parse(date)
    val intervals = parser.intervals(data, Some(anchor))

    assert(intervals(0)._1 == (93, 97))
    assert(intervals(0)._2(0)._1 === LocalDateTime.of(2015, 1, 1, 0, 0))
    assert(intervals(0)._2(0)._2 === LocalDateTime.of(2016, 1, 1, 0, 0))
    assert(intervals(0)._2(0)._3 === 31536000)
  }

  test("since-last") {
    val date = """since last March""".stripMargin
    val data = parser.parse(date)
    val intervals = parser.intervals(data, Some(anchor))

    assert(intervals(0)._1 == (0, 16))
    assert(intervals(0)._2(0)._1 === LocalDateTime.of(2018, 3, 1, 0, 0))
    assert(intervals(0)._2(0)._2 === LocalDateTime.of(2018, 7, 7, 0, 0))
    assert(intervals(0)._2(0)._3 === 11059200)
  }


  test("since-year") {
    val date =
      """A substantial decline in gas revenue since 2014 has contributed
        | to a sharp drop in both foreign currency reserves and the value
        | of the South Sudanese pound.""".stripMargin
    val data = parser.parse(date)
    val intervals = parser.intervals(data, Some(anchor))

    assert(intervals(0)._1 == (37, 47))
    assert(intervals(0)._2(0)._1 === LocalDateTime.of(2014, 1, 1, 0, 0))
    assert(intervals(0)._2(0)._2 === LocalDateTime.of(2018, 7, 7, 0, 0))
    assert(intervals(0)._2(0)._3 === 142387200)
  }
}
