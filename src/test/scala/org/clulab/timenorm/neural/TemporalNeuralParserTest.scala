package org.clulab.timenorm.neural

import java.time.LocalDateTime

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TemporalNeuralParserTest extends FunSuite {

  Thread.sleep(10000)

  val parser = new TemporalNeuralParser()
  val anchor = parser.dct(parser.parse(List("2018-07-06"))(0))
  // val anchor = parser.dct(parser.parse(List("2018"))(0))
  val test1 = "2018-10-10"
  //val test1 = "2018"
  val test2 = "January"
  val test3 = "last Friday"
  val test4 = """South Sudan receives a ranking of 186 out of 189 on
                |ease of doing business in the World Bank 2015 Doing
                |Business report -LRB- World Bank 2014 -RRB- .""".stripMargin
  val test5 = "since last March"
  val test6 = """A substantial decline in gas revenue since 2014 has
                |contributed to a sharp drop in both foreign currency
                |reserves and the value of the South Sudanese pound.""".stripMargin
  //val dates = List(test1, test2, test3, test4, test5, test6)
  val dates = List(test2, test2)
  val data = parser.parse(dates)
  val intervals = parser.intervals(data, Some(anchor))

  test("parse-interval") {
    assert(intervals(0)(0)._1 == (0, 10))
    assert(intervals(0)(0)._2(0)._1 === LocalDateTime.of(2018, 10, 10, 0, 0))
    assert(intervals(0)(0)._2(0)._2 === LocalDateTime.of(2018, 10, 11, 0, 0))
    assert(intervals(0)(0)._2(0)._3 === 86400)
  }

  test("parse-repeatingInterval") {
    assert(intervals(1)(0)._1 == (0, 7))
    assert(intervals(1)(0)._2(0)._1 === null)
    assert(intervals(1)(0)._2(0)._2 === null)
    assert(intervals(1)(0)._2(0)._3 === 2678400)

  }

  test("parse-last") {
    assert(intervals(2)(0)._1 == (0, 11))
    assert(intervals(2)(0)._2(0)._1 === LocalDateTime.of(2018, 6, 29, 0, 0))
    assert(intervals(2)(0)._2(0)._2 === LocalDateTime.of(2018, 6, 30, 0, 0))
    assert(intervals(2)(0)._2(0)._3 === 86400)
  }

  test("fill-not-complete-span") {
    assert(intervals(3)(0)._1 == (93, 97))
    assert(intervals(3)(0)._2(0)._1 === LocalDateTime.of(2015, 1, 1, 0, 0))
    assert(intervals(3)(0)._2(0)._2 === LocalDateTime.of(2016, 1, 1, 0, 0))
    assert(intervals(3)(0)._2(0)._3 === 31536000)
  }

  test("since-last") {
    assert(intervals(4)(0)._1 == (0, 16))
    assert(intervals(4)(0)._2(0)._1 === LocalDateTime.of(2018, 3, 1, 0, 0))
    assert(intervals(4)(0)._2(0)._2 === LocalDateTime.of(2018, 7, 7, 0, 0))
    assert(intervals(4)(0)._2(0)._3 === 11059200)
  }

  test("since-year") {
    assert(intervals(5)(0)._1 == (37, 47))
    assert(intervals(5)(0)._2(0)._1 === LocalDateTime.of(2014, 1, 1, 0, 0))
    assert(intervals(5)(0)._2(0)._2 === LocalDateTime.of(2018, 7, 7, 0, 0))
    assert(intervals(5)(0)._2(0)._3 === 142387200)
  }
}
