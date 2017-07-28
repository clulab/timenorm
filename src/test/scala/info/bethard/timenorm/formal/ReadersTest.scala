package info.bethard.timenorm.formal

import java.time.LocalDateTime
import java.time.temporal.ChronoField

import com.codecommit.antixml.{Elem, XML}
import info.bethard.anafora.Data
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReadersTest extends FunSuite {

  implicit def intervalEquality[T <: Interval] = new org.scalactic.Equality[T] {
    override def areEqual(a: T, b: Any): Boolean = b match {
      case Interval(bStart, bEnd) => a.start == bStart && a.end == bEnd
      case _ => false
    }
  }

  test("Noon") {
    val elem: Elem = XML.fromString(
      """
        |<data>
        |<annotations>
        |        <entity>
        |                <id>1@test</id>
        |                <span>0,4</span>
        |                <type>Year</type>
        |                <parentsType>Operator</parentsType>
        |                <properties>
        |                        <Value>2000</Value>
        |                        <Sub-Interval>2@test</Sub-Interval>
        |                </properties>
        |        </entity>
        |        <entity>
        |                <id>2@test</id>
        |                <span>5,7</span>
        |                <type>Month-Of-Year</type>
        |                <parentsType>Repeating-Interval</parentsType>
        |                <properties>
        |                        <Type>October</Type>
        |                        <Sub-Interval>3@test</Sub-Interval>
        |                        <Number></Number>
        |                        <Modifier></Modifier>
        |                </properties>
        |        </entity>
        |        <entity>
        |                <id>3@test</id>
        |                <span>8,10</span>
        |                <type>Day-Of-Month</type>
        |                <parentsType>Repeating-Interval</parentsType>
        |                <properties>
        |                        <Value>25</Value>
        |                        <Sub-Interval>4@test</Sub-Interval>
        |                        <Number></Number>
        |                        <Modifier></Modifier>
        |                </properties>
        |        </entity>
        |        <entity>
        |                <id>4@test</id>
        |                <span>11,15</span>
        |                <type>Part-Of-Day</type>
        |                <parentsType>Repeating-Interval</parentsType>
        |                <properties>
        |                        <Type>Noon</Type>
        |                        <Sub-Interval></Sub-Interval>
        |                        <Number></Number>
        |                        <Modifier></Modifier>
        |                </properties>
        |        </entity>
        |</annotations>`
        |</data>
      """.stripMargin)
    implicit val data = new Data(elem, "2000-10-25 noon")
    val temporals = data.entities.map(AnaforaReader.temporal)
    temporals match {
      case Seq(year: Interval, _: RepeatingInterval, _: RepeatingInterval, noon: RepeatingField) =>
        assert(year === SimpleInterval.of(2000, 10, 25, 12, 0))
        assert(noon.field === ChronoField.MINUTE_OF_DAY)
      case _ => fail("expected Seq(year: I, month: RI, day: RI, noon: RI), found " + temporals)
    }
  }
}
