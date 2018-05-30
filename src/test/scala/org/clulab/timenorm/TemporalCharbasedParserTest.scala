package org.clulab.timenorm

import scala.util.Success
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.temporal.ChronoUnit._
import java.time.LocalDate

@RunWith(classOf[JUnitRunner])
class TemporalCharbasedParserTest extends FunSuite {

  val modelPath = this.getClass.getResource("/org/clulab/timenorm/model/char-3softmax-extra/lstm_models_2features.hdf5").getPath()
  val parser = new TemporalCharbasedParser(modelPath)
  val now = LocalDate.now()
  val anchor = TimeSpan.of(now.getYear, now.getMonthValue, now.getDayOfMonth)

  test("parse-interval") {
    val date = "2018-10-10"
    val data = parser.parse("\n\n\n" + date + "\n\n\n", anchor)
    for (ti <- parser.intervals(data); i <- ti._2) {
      println(s"${ti._1._1}, ${ti._1._2} ${i._1} ${i._2}")
    }
  }

  test("parse-repeatingInterval") {
    val date = "January"
    val data = parser.parse("\n\n\n" + date + "\n\n\n", anchor)
    for (ti <- parser.intervals(data); i <- ti._2) {
      println(s"${ti._1._1}, ${ti._1._2} ${i._1} ${i._2}")
    }
  }

  test("parse-error") {
    val date = "Friday."
    val data = parser.parse("\n\n\n" + date + "\n\n\n", anchor)
    for (ti <- parser.intervals(data); i <- ti._2) {
      println(s"${ti._1._1}, ${ti._1._2} ${i._1} ${i._2}")
    }
  }

}
