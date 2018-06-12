package org.clulab.timenorm

import scala.util.Success
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.temporal.ChronoUnit._
import java.time.LocalDateTime

@RunWith(classOf[JUnitRunner])
class TemporalCharbasedParserTest extends FunSuite {

  val modelPath = this.getClass.getResource("/org/clulab/timenorm/model/char-3softmax-extra/lstm_models_2features.hdf5").getPath()
  val parser = new TemporalCharbasedParser(modelPath)
  val now = LocalDateTime.now()
  val anchor = TimeSpan.of(now.getYear, now.getMonthValue, now.getDayOfMonth)

  test("parse-interval") {
    val date = "2018-10-10"
    val data = parser.parse(date, anchor)

    assert(parser.intervals(data)(0)._1 == (0, 10))
    assert(parser.intervals(data)(0)._2(0)._1 === LocalDateTime.of(2018, 10, 10, 0, 0))
    assert(parser.intervals(data)(0)._2(0)._2 === 86400)
  }

  test("parse-repeatingInterval") {
    val date = "January"
    val data = parser.parse(date, anchor)

    assert(parser.intervals(data)(0)._1 == (0, 7))
    assert(parser.intervals(data)(0)._2(0)._1 === null)
    assert(parser.intervals(data)(0)._2(0)._2 === 2678400)
  }

  test("parse-undef") {
    val date = "Friday."
    val data = parser.parse(date, anchor)

    assert(parser.intervals(data)(0)._1 == (0, 6))
    assert(parser.intervals(data)(0)._2(0)._1 === null)
    assert(parser.intervals(data)(0)._2(0)._2 === 0)
  }

}
