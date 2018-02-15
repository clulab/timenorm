package org.clulab.timenorm.formal

import java.time.LocalDateTime
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit.{DAYS}

import com.codecommit.antixml._
import org.clulab.anafora.Data
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReadersTest extends FunSuite with TypesSuite {

  test("Noon") {
    val elem =
      <data>
        <annotations>
          <entity>
            <id>1@test</id>
            <span>0,4</span>
            <type>Year</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Value>2000</Value>
              <Sub-Interval>2@test</Sub-Interval>
            </properties>
          </entity>
          <entity>
            <id>2@test</id>
            <span>5,7</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>October</Type>
              <Sub-Interval>3@test</Sub-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>3@test</id>
            <span>8,10</span>
            <type>Day-Of-Month</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Value>25</Value>
              <Sub-Interval>4@test</Sub-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>4@test</id>
            <span>11,15</span>
            <type>Part-Of-Day</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>Noon</Type>
              <Sub-Interval></Sub-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = new Data(elem, Some("2000-10-25 noon"))

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
    val xml =
      <data>
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

    implicit val data = Data(xml, Some("first nine months of 1997"))

    val dct = SimpleInterval.of(1998, 2, 6, 22, 19)
    var aReader = new AnaforaReader(dct)

    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(nth: NthRIs, number: Number, month: RepeatingInterval, year: Year) =>
        assert(nth === SimpleIntervals((1 to 9).map(m => SimpleInterval.of(1997, m))))
      case _ => fail("expected Seq(nth: NRIs, number: N, month: RI, year: Y), found " + temporals)
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
    implicit val data = Data(xml, Some("last few months"))
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
    implicit val data = Data(xml, Some("19980331"))
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
              <Semantics>Interval-Included</Semantics>
              <Interval-Type>DocTime</Interval-Type>
              <Interval></Interval>
              <Period></Period>
              <Repeating-Interval>117@e@APW19980306.1001@gold</Repeating-Interval>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, Some("Friday"))
    val start = LocalDateTime.of(1998, 3, 6, 0, 0)
    val dct = SimpleInterval(start, start.plusDays(1))
    val aReader = new AnaforaReader(dct)
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(friday: RepeatingInterval, last: LastRI) => assert(last === SimpleInterval(start, start.plusDays(1)))
      case _ => fail("expected Seq(friday: RI, last: Last), found " + temporals)
    }
  }

  test("APW19980322.0749 (3918,3925) Earlier Sunday") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>125@e@APW19980322.0749@gold</id>
            <span>3926,3932</span>
            <type>Day-Of-Week</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>Sunday</Type>
              <Sub-Interval></Sub-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>177@e@APW19980322.0749@gold</id>
            <span>3926,3932</span>
            <type>Last</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Semantics>Interval-Included</Semantics>
              <Interval-Type>DocTime</Interval-Type>
              <Interval></Interval>
              <Period></Period>
              <Repeating-Interval>125@e@APW19980322.0749@gold</Repeating-Interval>
            </properties>
          </entity>
          <entity>
            <id>178@e@APW19980322.0749@gold</id>
            <span>3918,3925</span>
            <type>Before</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Interval-Type>Link</Interval-Type>
              <Interval>179@e@APW19980322.0749@gold</Interval>
              <Period></Period>
              <Repeating-Interval></Repeating-Interval>
              <Semantics>Interval-Not-Included</Semantics>
            </properties>
          </entity>
          <entity>
            <id>179@e@APW19980322.0749@gold</id>
            <span>3750,3759</span>
            <type>Event</type>
            <parentsType>Other</parentsType>
            <properties>
            </properties>
          </entity>
          <entity>
            <id>180@e@APW19980322.0749@gold</id>
            <span>3918,3925</span>
            <type>Intersection</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Intervals>178@e@APW19980322.0749@gold</Intervals>
              <Intervals>177@e@APW19980322.0749@gold</Intervals>
              <Repeating-Intervals></Repeating-Intervals>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, None)
    val aReader = new AnaforaReader(SimpleInterval.of(1998, 3, 22))
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(_: RepeatingInterval, last: LastRI, _: BeforeP, _: Event, intersection: IntersectionI) =>
        assert(last === SimpleInterval.of(1998, 3, 22))
        assert(!intersection.isDefined)
      case _ => fail("expected Seq(sunday: RI, last: I, before: I, event: I, intersection: I), found " + temporals)
    }
  }

  test("APW19980219.0476 (1164,1171) January this year") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>164@e@APW19980219.0476@gold</id>
            <span>1164,1171</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>January</Type>
              <Sub-Interval></Sub-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>166@e@APW19980219.0476@gold</id>
            <span>1177,1181</span>
            <type>Calendar-Interval</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>Year</Type>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>185@e@APW19980219.0476@gold</id>
            <span>1172,1176</span>
            <type>This</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Interval-Type>DocTime</Interval-Type>
              <Interval></Interval>
              <Period></Period>
              <Repeating-Interval>166@e@APW19980219.0476@gold</Repeating-Interval>
            </properties>
          </entity>
          <entity>
            <id>200@e@APW19980219.0476@gold</id>
            <span>1164,1171</span>
            <type>Intersection</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Intervals>185@e@APW19980219.0476@gold</Intervals>
              <Repeating-Intervals>164@e@APW19980219.0476@gold</Repeating-Intervals>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, None)
    val aReader = new AnaforaReader(SimpleInterval.of(1998, 2, 19))
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(_: RepeatingInterval, _: RepeatingInterval, _: ThisRI, intersection: Intervals) =>
        assert(intersection === SimpleIntervals(Seq(SimpleInterval.of(1998, 1))))
      case _ => fail("expected Seq(january: RI, year: RI, this: I, intersection: Is), found " + temporals)
    }
  }

  test("NYT19980206.0460 (1095,1110) quarter-century") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>307@e@NYT19980206.0460@gold</id>
            <span>1095,1110</span>
            <type>Calendar-Interval</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>Quarter-Century</Type>
              <Number/>
              <Modifier/>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, None)
    val dct = SimpleInterval.of(1998, 2, 6)
    val aReader = new AnaforaReader(dct)
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(quarterCentury: RepeatingInterval) =>
        assert(ThisRI(dct, quarterCentury)
          === SimpleInterval(LocalDateTime.of(1975, 1, 1, 0, 0), LocalDateTime.of(2000, 1, 1, 0, 0)))
      case _ => fail("expected Seq(quarter-century: RI), found " + temporals)
    }
  }

  test("wsj_0266 (489,496) 20th century") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>27@e@wsj_0266@isma5916</id>
            <span>489,496</span>
            <type>Calendar-Interval</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>Century</Type>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>28@e@wsj_0266@isma5916</id>
            <span>484,488</span>
            <type>NthFromStart</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Interval-Type>DocTime-Era</Interval-Type>
              <Interval></Interval>
              <Value>20</Value>
              <Period></Period>
              <Repeating-Interval>27@e@wsj_0266@isma5916</Repeating-Interval>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, None)
    val dct = SimpleInterval.of(1989, 11, 1)
    val aReader = new AnaforaReader(dct)
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(_: RepeatingInterval, nth: NthRI) =>
        assert(nth === SimpleInterval(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.of(2000, 1, 1, 0, 0)))
      case _ => fail("expected Seq(century: RI, 20th: I), found " + temporals)
    }
  }

  test("wsj_0348 (293,300) third quarter") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>23@e@wsj_0348@isma5916</id>
            <span>293,300</span>
            <type>Calendar-Interval</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>Quarter-Year</Type>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>24@e@wsj_0348@isma5916</id>
            <span>287,292</span>
            <type>NthFromStart</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Interval-Type>DocTime-Year</Interval-Type>
              <Interval></Interval>
              <Value>3</Value>
              <Period></Period>
              <Repeating-Interval>23@e@wsj_0348@isma5916</Repeating-Interval>
            </properties>
          </entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, None)
    val dct = SimpleInterval.of(1989, 11, 1)
    val aReader = new AnaforaReader(dct)
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(_: RepeatingInterval, nth: NthRI) =>
        assert(nth === SimpleInterval(LocalDateTime.of(1989, 7, 1, 0, 0), LocalDateTime.of(1989, 10, 1, 0, 0)))
      case _ => fail("expected Seq(quarter: RI, 3rd: I), found " + temporals)
    }
  }

  test("ID036_clinic_108 (6422,6424) 00") {
    val xml =
      <data>
        <annotations>
         <entity>
          <id>899@e@ID036_clinic_108@gold</id>
          <span>6422,6424</span>
	  <type>Two-Digit-Year</type>
	  <parentsType>Operator</parentsType>
	  <properties>
	   <Interval-Type>DocTime</Interval-Type>
	   <Interval></Interval>
	   <Value>00</Value>
	   <Sub-Interval></Sub-Interval>
	  </properties>
	</entity>
        </annotations>
      </data>.convert
    implicit val data = Data(xml, None)
    val dct = SimpleInterval.of(2010, 8, 5)
    val aReader = new AnaforaReader(dct)
    val temporals = data.entities.map(aReader.temporal)
    temporals match {
      case Seq(year: YearSuffix) =>
        assert(year
          === SimpleInterval.of(2000))
      case _ => fail("expected Seq(year: YearSuffix), found " + temporals)
    }
  }
}
