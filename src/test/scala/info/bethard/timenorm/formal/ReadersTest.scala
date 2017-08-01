package info.bethard.timenorm.formal

import java.time.temporal.ChronoField

import com.codecommit.antixml._
import info.bethard.anafora.Data
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReadersTest extends FunSuite with TypesSuite {

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

  test("NYT19980206.0460 (2979,3004) first nine months of 1997") {
    val xml = <data>
      <annotations>
        <entity>
          <id>206@e@NYT19980206.0460@gold</id>
          <span>0,5</span>
          <type>NthFromStart</type>
          <parentsType>Operator</parentsType>
          <properties>
            <Interval-Type>Link</Interval-Type>
            <Interval>209@e@NYT19980206.0460@gold</Interval>
            <Value>1</Value>
            <Period></Period>
            <Repeating-Interval>208@e@NYT19980206.0460@gold</Repeating-Interval>
          </properties>
        </entity>
        <entity>
          <id>207@e@NYT19980206.0460@gold</id>
          <span>6,10</span>
          <type>Number</type>
          <parentsType>Other</parentsType>
          <properties>
            <Value>9</Value>
          </properties>
        </entity>
        <entity>
          <id>208@e@NYT19980206.0460@gold</id>
          <span>11,17</span>
          <type>Calendar-Interval</type>
          <parentsType>Repeating-Interval</parentsType>
          <properties>
            <Type>Month</Type>
            <Number>207@e@NYT19980206.0460@gold</Number>
            <Modifier></Modifier>
          </properties>
        </entity>
        <entity>
          <id>209@e@NYT19980206.0460@gold</id>
          <span>21,25</span>
          <type>Year</type>
          <parentsType>Interval</parentsType>
          <properties>
            <Value>1997</Value>
            <Sub-Interval></Sub-Interval>
            <Modifier></Modifier>
          </properties>
        </entity>
      </annotations>
    </data>.convert

    implicit val data = Data(xml, "first nine months of 1997")
    val temporals = data.entities.map(AnaforaReader.temporal)
    temporals match {
      case Seq(nth: NthFromStartRIs, number: Number, month: RepeatingInterval, year: Year) =>
        assert(nth === SimpleIntervals((1 to 9).map(m => SimpleInterval.of(1997, m))))
      case _ => fail("expected Seq(nth: NFSRIs, number: N, month: RI, year: Y), found " + temporals)
    }
  }
}
