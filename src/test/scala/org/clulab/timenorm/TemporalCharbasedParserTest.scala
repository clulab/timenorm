package org.clulab.timenorm

import scala.util.Success
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.temporal.ChronoUnit._
import java.time.LocalDateTime
import com.codecommit.antixml._

@RunWith(classOf[JUnitRunner])
class TemporalCharbasedParserTest extends FunSuite {

  val modelPath = this.getClass.getResource("/org/clulab/timenorm/model/char-3softmax-extra/lstm_models_2features.hdf5").getPath()
  val parser = new TemporalCharbasedParser(modelPath)
  val anchor = TimeSpan.of(2018, 7, 6)

  test("parse-interval") {
    val date = "2018-10-10"
    val data = parser.parse(date, anchor)

    assert(parser.intervals(data)(0)._1 == (0, 10))
    assert(parser.intervals(data)(0)._2(0)._1 === LocalDateTime.of(2018, 10, 10, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._2 === LocalDateTime.of(2018, 10, 11, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._3 === 86400)
  }

  test("parse-repeatingInterval") {
    val date = "January"
    val data = parser.parse(date, anchor)

    assert(parser.intervals(data)(0)._1 == (0, 7))
    assert(parser.intervals(data)(0)._2(0)._1 === null)
    assert(parser.intervals(data)(0)._2(0)._2 === null)
    assert(parser.intervals(data)(0)._2(0)._3 === 2678400)
  }

  test("parse-undef") {
    val date = "Friday."
    val data = parser.parse(date, anchor)

    assert(parser.intervals(data)(0)._1 == (0, 6))
    assert(parser.intervals(data)(0)._2(0)._1 === LocalDateTime.of(2018, 6, 29, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._2 === LocalDateTime.of(2018, 6, 30, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._3 === 86400)
  }

  test("fill-not-complete-span") {
    val date = "South Sudan receives a ranking of 186 out of 189 on ease of doing business in the World Bank 2015 Doing Business report -LRB- World Bank 2014 -RRB- ."
    val data = parser.parse(date, anchor)

    assert(parser.intervals(data)(0)._1 == (93, 97))
    assert(parser.intervals(data)(0)._2(0)._1 === LocalDateTime.of(2015, 1, 1, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._2 === LocalDateTime.of(2016, 1, 1, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._3 === 31536000)
  }

  test("since-year") {
    val date = "A substantial decline in gas revenue since 2014 has contributed to a sharp drop in both foreign currency reserves and the value of the South Sudanese pound."

    val data = parser.parse(date, anchor)

    assert(parser.intervals(data)(0)._1 == (37, 47))
    assert(parser.intervals(data)(0)._2(0)._1 === LocalDateTime.of(2014, 1, 1, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._2 === LocalDateTime.of(2018, 7, 7, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._3 === 142387200)
  }
}
