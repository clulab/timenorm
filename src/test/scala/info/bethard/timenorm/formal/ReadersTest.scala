package info.bethard.timenorm.formal

import java.time.LocalDateTime
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit.{DAYS}

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

    val dct = SimpleInterval.of(1998, 2, 13, 15, 44)
    var aReader = new AnaforaReader(dct)

    val temporals = data.entities.map(aReader.temporal)
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

    val dct = SimpleInterval.of(1998, 2, 6, 22, 19)
    var aReader = new AnaforaReader(dct)

    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(nth: NthFromStartRIs, number: Number, month: RepeatingInterval, year: Year) =>
        assert(nth === SimpleIntervals((1 to 9).map(m => SimpleInterval.of(1997, m))))
      case _ => fail("expected Seq(nth: NFSRIs, number: N, month: RI, year: Y), found " + temporals)
    }
  }

  test("NYT19980206.0460 (3441,3445) last few months") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>210@e@NYT19980206.0460@gold</id>
            <span>0,4</span>
            <type>Last</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Semantics>Standard</Semantics>
              <Interval-Type>DocTime</Interval-Type>
              <Interval></Interval>
              <Period></Period>
              <Repeating-Interval>212@e@NYT19980206.0460@gold</Repeating-Interval>
            </properties>
          </entity>
          <entity>
            <id>211@e@NYT19980206.0460@gold</id>
            <span>5,8</span>
            <type>Number</type>
            <parentsType>Other</parentsType>
            <properties>
              <Value>?</Value>
            </properties>
          </entity>
          <entity>
            <id>212@e@NYT19980206.0460@gold</id>
            <span>9,15</span>
            <type>Calendar-Interval</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>Month</Type>
              <Number>211@e@NYT19980206.0460@gold</Number>
              <Modifier></Modifier>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, "last few months")
    val start = LocalDateTime.now().truncatedTo(DAYS)
    val dct = SimpleInterval(start, start.plusDays(1))
    val aReader = new AnaforaReader(dct)
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(last: Last, _: Number, _: RepeatingInterval) => assert(!last.isDefined)
      case _ => fail("expected Seq(last: Last, number: N, month: RI), found " + temporals)
    }
  }

  test("VOA19980331.1700.1533 (25,29) 19980331") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>70@e@VOA19980331.1700.1533@gold</id>
            <span>0,4</span>
            <type>Year</type>
            <parentsType>Interval</parentsType>
            <properties>
              <Value>1998</Value>
              <Sub-Interval>65@e@VOA19980331.1700.1533@gold</Sub-Interval>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>65@e@VOA19980331.1700.1533@gold</id>
            <span>4,6</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>March</Type>
              <Sub-Interval>61@e@VOA19980331.1700.1533@gold</Sub-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>61@e@VOA19980331.1700.1533@gold</id>
            <span>6,8</span>
            <type>Day-Of-Month</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Value>31</Value>
              <Sub-Interval></Sub-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, "19980331")
    val start = LocalDateTime.of(1998, 3, 31, 0, 0)
    val dct = SimpleInterval(start, start.plusDays(1))
    val aReader = new AnaforaReader(dct)
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(year: Interval, _: RepeatingInterval, _: RepeatingInterval) =>
        assert(year === SimpleInterval.of(1998, 3, 31))
      case _ => fail("expected Seq(year: Y, month: RI, day: RI), found " + temporals)
    }
  }

  test("APW19980306.1001 (1705,1711) Friday") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>117@e@APW19980306.1001@gold</id>
            <span>0,6</span>
            <type>Day-Of-Week</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>Friday</Type>
              <Sub-Interval></Sub-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>118@e@APW19980306.1001@gold</id>
            <span>0,6</span>
            <type>Last</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Semantics>Newswire</Semantics>
              <Interval-Type>DocTime</Interval-Type>
              <Interval></Interval>
              <Period></Period>
              <Repeating-Interval>117@e@APW19980306.1001@gold</Repeating-Interval>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, "Friday")
    val start = LocalDateTime.of(1998, 3, 6, 0, 0)
    val dct = SimpleInterval(start, start.plusDays(1))
    val aReader = new AnaforaReader(dct)
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(friday: RepeatingInterval, last: LastFromEndRI) => assert (last === SimpleInterval(start, start.plusDays(1)))
      case _ => fail("expected Seq(friday: RI, last: Last), found " + temporals)
    }
  }
}
