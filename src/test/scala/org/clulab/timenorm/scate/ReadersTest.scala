package org.clulab.timenorm.scate

import java.time.LocalDateTime
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit

import org.clulab.anafora.Data
import org.scalatest.FunSuite

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
      </data>
    implicit val data: Data = new Data(elem, Some("2000-10-25 noon"))
    val dct = SimpleInterval.of(1998, 2, 13, 15, 44)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(year: ThisRI, month: IntersectionRI, day: IntersectionRI, noon: RepeatingField) =>
        assert(year === SimpleInterval.of(2000, 10, 25, 12, 0))
        assert(year.charSpan === Some((0, 15)))
        assert(month.following(dct.end).next() === SimpleInterval.of(1998, 10, 25, 12, 0))
        assert(month.charSpan === Some((5, 15)))
        assert(day.following(dct.end).next() === SimpleInterval.of(1998, 2, 25, 12, 0))
        assert(day.charSpan === Some((8, 15)))
        assert(noon.field === ChronoField.MINUTE_OF_DAY)
        assert(noon.charSpan === Some((11, 15)))
        // ensure intersections are not nested
        for (intersectionRI <- Seq(month, day)) {
          assert(intersectionRI.repeatingIntervals.collect { case ri: IntersectionRI => ri }.isEmpty)
        }
      case other => fail(f"expected Seq(year: I, month: RI, day: RI, noon: RI), found $other")
    }
  }

  test("NoonSuperInterval") {
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
            </properties>
          </entity>
          <entity>
            <id>2@test</id>
            <span>5,7</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>October</Type>
              <Super-Interval>1@test</Super-Interval>
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
              <Super-Interval>2@test</Super-Interval>
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
              <Super-Interval>3@test</Super-Interval>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
        </annotations>
      </data>
    implicit val data: Data = new Data(elem, Some("2000-10-25 noon"))
    val dct = SimpleInterval.of(1998, 2, 13, 15, 44)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(year: Year, month: ThisRI, day: ThisRI, noon: ThisRI) =>
        assert(year === SimpleInterval.of(2000))
        assert(year.charSpan === Some((0, 4)))
        assert(month === SimpleInterval.of(2000, 10))
        assert(month.charSpan === Some((0, 7)))
        assert(day === SimpleInterval.of(2000, 10, 25))
        assert(day.charSpan === Some((0, 10)))
        assert(noon === SimpleInterval.of(2000, 10, 25, 12, 0))
        assert(noon.charSpan === Some((0, 15)))
        // ensure intersections are not nested
        for (thisRI <- Seq(day, noon)) {
          val IntersectionRI(repeatingIntervals, _) = thisRI.repeatingInterval
          assert(repeatingIntervals.collect{ case ri: IntersectionRI => ri}.isEmpty)
        }
      case other => fail(f"expected Seq(year: I, month: I, day: I, noon: I), found $other")
    }
  }

  test (testName = "after December 2017") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>0@e@Doc9@gold</id>
            <span>0,5</span>
            <type>After</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Semantics>Interval-Not-Included</Semantics>
              <!--TODO: ASK the difference between interval/repeating interval for operators-->
              <Interval-Type>Link</Interval-Type>
              <Interval>1@e@Doc9@gold</Interval>
              <Period></Period>
              <Repeating-Interval></Repeating-Interval>
            </properties>
          </entity>
          <entity>
            <id>1@e@Doc9@gold</id>
            <span>6,14</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>December</Type>
              <Number></Number>
              <Modifier></Modifier>
              <Super-Interval>2@e@Doc9@gold</Super-Interval>
            </properties>
          </entity>
          <entity>
            <id>2@e@Doc9@gold</id>
            <span>15,19</span>
            <type>Year</type>
            <parentsType>Interval</parentsType>
            <properties>
              <Value>2017</Value>
              <Modifier></Modifier>
            </properties>
          </entity>
        </annotations>
      </data>
      implicit val data: Data = Data(xml, Some("after December 2017"))
      val dct = SimpleInterval.of(1998, 2, 6, 22, 19)
      data.entities.map(new AnaforaReader(dct).temporal) match {
        case Seq(after: AfterP, month: ThisRI, year: Year) =>
          assert(after.charSpan === Some((0, 19)))
          assert(!after.isDefined)
          assert(month === SimpleInterval.of(2017, 12))
          assert(month.charSpan === Some((6, 19)))
          assert(year === SimpleInterval.of(2017))
          assert(year.charSpan === Some((15, 19)))
         case other => fail(f"expected Seq(after: I, month: I, year: I), found $other")
    }
  }
  test (testName = "last December 25") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>1@e@Doc9@gold</id>
            <span>0,4</span>
            <type>Last</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Semantics>Interval-Not-Included</Semantics>
              <Interval-Type>DocTime</Interval-Type>
              <Interval></Interval>
              <Period></Period>
              <Repeating-Interval>2@e@Doc9@gold</Repeating-Interval>
            </properties>
          </entity>
          <entity>
            <id>1@e@Doc9@gold</id>
            <span>5,13</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>December</Type>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>2@e@Doc9@gold</id>
            <span>14,16</span>
            <type>Day-Of-Month</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Value>25</Value>
              <Modifier></Modifier>
              <Super-Interval>1@e@Doc9@gold</Super-Interval>
            </properties>
          </entity>
        </annotations>
      </data>
    implicit val data: Data = Data(xml, Some("last December 25"))
    val dct = SimpleInterval.of(2018, 2, 6, 22, 19)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(last: LastRI, month: RepeatingField, day: IntersectionRI) =>
        assert(last === SimpleInterval.of(2017, 12, 25))
        assert(last.charSpan === Some((0, 16)))
        assert(month.following(dct.end).next === SimpleInterval.of(2018, 12))
        assert(month.charSpan === Some((5, 13)))
        assert(day.following(dct.end).next === SimpleInterval.of(2018, 12, 25))
        assert(day.charSpan === Some((5, 16)))
      case other => fail(f"expected Seq(year: I, month: I, day: I, noon: I), found $other")
    }
  }

  test (testName = "this December 25") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>0@e@Doc9@gold</id>
            <span>0,4</span>
            <type>This</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Interval-Type>DocTime</Interval-Type>
              <Interval></Interval>
              <Period></Period>
              <Repeating-Interval>2@e@Doc9@gold</Repeating-Interval>
            </properties>
          </entity>
          <entity>
            <id>1@e@Doc9@gold</id>
            <span>5,13</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>December</Type>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>2@e@Doc9@gold</id>
            <span>14,16</span>
            <type>Day-Of-Month</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Value>25</Value>
              <Modifier></Modifier>
              <Super-Interval>1@e@Doc9@gold</Super-Interval>
            </properties>
          </entity>
        </annotations>
      </data>
    implicit val data: Data = Data(xml, Some("this December 25"))
    val dct = SimpleInterval.of(2018, 2, 6, 22, 19)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(thisInterval: ThisRI, month: RepeatingField, day: IntersectionRI) =>
        assert(thisInterval === SimpleInterval.of(2018, 12, 25))
        assert(thisInterval.charSpan === Some((0, 16)))
        assert(month.following(dct.end).next === SimpleInterval.of(2018, 12))
        assert(month.charSpan === Some((5, 13)))
        assert(day.following(dct.end).next === SimpleInterval.of(2018, 12, 25))
        assert(day.charSpan === Some((5, 16)))
      case other => fail(f"expected Seq(year: I, month: I, day: I, noon: I), found $other")
    }
  }

  test (testName = "December 17") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>1@e@Doc9@gold</id>
            <span>0,8</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>December</Type>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>2@e@Doc9@gold</id>
            <span>9,11</span>
            <type>Day-Of-Month</type>
            <parentsType>Interval</parentsType>
            <properties>
              <Value>17</Value>
              <Modifier></Modifier>
              <Super-Interval>1@e@Doc9@gold</Super-Interval>
            </properties>
          </entity>
        </annotations>
      </data>
    implicit val data: Data = Data(xml, Some("December 17"))
    val dct = SimpleInterval.of(1998, 2, 6, 22, 19)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(month: RepeatingField, day: IntersectionRI) =>
        assert(month.charSpan === Some((0, 8)))
        assert(month.following(dct.end).next === SimpleInterval.of(1998, 12))
        assert(day.charSpan === Some((0, 11)))
        assert(day.following(dct.end).next === SimpleInterval.of(1998, 12, 17))
      case other => fail(f"expected Seq(month: RI, day: RI), found $other")
    }
  }
  test (testName = "December 2017 to January 2018") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>1@e@Doc9@gold</id>
            <span>0,8</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>December</Type>
              <Number></Number>
              <Modifier></Modifier>
              <Super-Interval>2@e@Doc9@gold</Super-Interval>
            </properties>
          </entity>
          <entity>
            <id>2@e@Doc9@gold</id>
            <span>9,13</span>
            <type>Year</type>
            <parentsType>Interval</parentsType>
            <properties>
              <Value>2017</Value>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>3@e@Doc9@gold</id>
            <span>14,16</span>
            <type>Between</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Start-Interval-Type>Link</Start-Interval-Type>
              <Start-Interval>1@e@Doc9@gold</Start-Interval>
              <Start-Included>Included</Start-Included>
              <End-Interval-Type>Link</End-Interval-Type>
              <End-Interval>4@e@Doc9@gold</End-Interval>
              <End-Included>Included</End-Included>
            </properties>
          </entity>
          <entity>
            <id>4@e@Doc9@gold</id>
            <span>17,24</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>January</Type>
              <Number></Number>
              <Modifier></Modifier>
              <Super-Interval>5@e@Doc9@gold</Super-Interval>
            </properties>
          </entity>
          <entity>
            <id>5@e@Doc9@gold</id>
            <span>25,29</span>
            <type>Year</type>
            <parentsType>Interval</parentsType>
            <properties>
              <Value>2018</Value>
              <Modifier></Modifier>
            </properties>
          </entity>
        </annotations>
      </data>
    implicit val data: Data = Data(xml, Some("December 2017 to January 2018"))
    val dct = SimpleInterval.of(1998, 2, 6, 22, 19)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(dec: ThisRI, y2017: Year, between: Between, jan: ThisRI, y2018: Year) =>
        assert(dec === SimpleInterval.of(2017, 12))
        assert(dec.charSpan === Some((0, 13)))
        assert(y2017 === SimpleInterval.of(2017))
        assert(y2017.charSpan === Some((9, 13)))
        assert(between === SimpleInterval(dec.start, jan.end))
        assert(between.charSpan === Some((0, 29)))
        assert(jan === SimpleInterval.of(2018, 1))
        assert(jan.charSpan === Some((17, 29)))
        assert(y2018 === SimpleInterval.of(2018))
        assert(y2018.charSpan === Some((25, 29)))
      case other => fail(f"expected Seq(year: I, month: I, day: I, noon: I), found $other")
    }
  }

  test (testName = "December 17 and 18") {
    val xml =
      <data>
        <annotations>
          <entity>
            <id>1@e@Doc9@gold</id>
            <span>0,8</span>
            <type>Month-Of-Year</type>
            <parentsType>Repeating-Interval</parentsType>
            <properties>
              <Type>December</Type>
              <Number></Number>
              <Modifier></Modifier>
            </properties>
          </entity>
          <entity>
            <id>2@e@Doc9@gold</id>
            <span>9,11</span>
            <type>Day-Of-Month</type>
            <parentsType>Interval</parentsType>
            <properties>
              <Value>17</Value>
              <Modifier></Modifier>
              <Super-Interval>1@e@Doc9@gold</Super-Interval>
            </properties>
          </entity>
          <entity>
            <id>3@e@Doc9@gold</id>
            <span>16,18</span>
            <type>Day-Of-Month</type>
            <parentsType>Interval</parentsType>
            <properties>
              <Value>18</Value>
              <Modifier></Modifier>
              <Super-Interval>1@e@Doc9@gold</Super-Interval>
            </properties>
          </entity>
          <entity>
            <id>4@e@Doc8@gold</id>
            <span>12,15</span>
            <type>Union</type>
            <parentsType>Operator</parentsType>
            <properties>
              <Repeating-Intervals>2@e@Doc9@gold</Repeating-Intervals>
              <Repeating-Intervals>3@e@Doc9@gold</Repeating-Intervals>
            </properties>
          </entity>
        </annotations>
      </data>
    implicit val data: Data = Data(xml, Some("December 17 and 18"))
    val dct = SimpleInterval.of(1998, 2, 6, 22, 19)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(dec: RepeatingField, day17: IntersectionRI, day18: IntersectionRI, union: UnionRI) =>
        assert(dec.following(dct.end).next === SimpleInterval.of(1998, 12))
        assert(dec.charSpan === Some((0, 8)))
        assert(day17.following(dct.end).next === SimpleInterval.of(1998, 12, 17))
        assert(day17.charSpan === Some((0,11)))
        assert(union.following(dct.end).take(4).toSeq === Seq(
          SimpleInterval.of(1998, 12, 17),
          SimpleInterval.of(1998, 12, 18),
          SimpleInterval.of(1999, 12, 17),
          SimpleInterval.of(1999, 12, 18)
        ))
        assert(union.charSpan === Some((0,18)))
        assert(day18.following(dct.end).next === SimpleInterval.of(1998, 12, 18))
        assert(day18.charSpan === Some((0,18)))
      case other => fail(f"expected Seq(month: RI, day1: RI, union: RI, day2: RI), found $other")
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
      </data>
    implicit val data: Data = Data(xml, Some("first nine months of 1997"))
    val dct = SimpleInterval.of(1998, 2, 6, 22, 19)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(nth: NthRIs, number: IntNumber, month: RepeatingUnit, year: Year) =>
        assert(nth === SimpleIntervals((1 to 9).map(m => SimpleInterval.of(1997, m))))
        assert(nth.charSpan === Some((0, 25)))
        assert(number.n === 9)
        assert(number.charSpan === Some((6, 10)))
        assert(month.unit === ChronoUnit.MONTHS)
        assert(month.following(dct.end).next === SimpleInterval.of(1998, 3))
        // we currently don't attach Numbers to RepeatingIntervals
        // assert(month.charSpan === Some((6, 17)))
        assert(year === SimpleInterval.of(1997))
        assert(year.charSpan === Some((21, 25)))
      case other => fail(f"expected Seq(nth: NRIs, number: N, month: RI, year: Y), found $other")
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
      </data>
    implicit val data: Data = Data(xml, Some("last few months"))
    val dct = SimpleInterval.of(2021, 6, 24)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(last: Last, number: VagueNumber, months: RepeatingUnit) =>
        assert(!last.isDefined)
        assert(last.charSpan === Some((0, 15)))
        assert(!number.isDefined)
        assert(number.charSpan === Some((5, 8)))
        assert(months.unit === ChronoUnit.MONTHS)
        assert(months.following(dct.end).next === SimpleInterval.of(2021, 7))
        assert(months.charSpan === Some((9, 15)))
      case other => fail(f"expected Seq(last: Last, number: N, month: RI), found $other")
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
      </data>
    implicit val data: Data = Data(xml, Some("19980331"))
    val dct = SimpleInterval.of(2021, 6, 24)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(year: ThisRI, month: IntersectionRI, day: RepeatingField) =>
        assert(year === SimpleInterval.of(1998, 3, 31))
        assert(year.charSpan === Some((0, 8)))
        assert(month.following(dct.end).next === SimpleInterval.of(2022, 3, 31))
        assert(month.charSpan === Some((4, 8)))
        assert(day.following(dct.end).next === SimpleInterval.of(2021, 7, 31))
        assert(day.charSpan === Some((6, 8)))
      case other => fail(f"expected Seq(year: Y, month: RI, day: RI), found $other")
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
      </data>
    implicit val data: Data = Data(xml, Some("Friday"))
    val dct = SimpleInterval.of(1998, 3, 6)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(friday: RepeatingField, last: LastRI) =>
        assert(last === dct)
        assert(last.charSpan === Some((0, 6)))
        assert(friday.following(dct.end).next === SimpleInterval.of(1998, 3, 13))
        assert(friday.charSpan === Some((0, 6)))
      case other => fail(f"expected Seq(friday: RI, last: Last), found $other")
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
      </data>
    implicit val data: Data = Data(xml, None)
    val dct = SimpleInterval.of(1998, 3, 22)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(sunday: RepeatingField, last: LastRI, before: BeforeP, event: Event, intersection: IntersectionI) =>
        assert(sunday.following(dct.end).next === SimpleInterval.of(1998, 3, 29))
        assert(sunday.charSpan === Some((3926,3932)))
        assert(last === SimpleInterval.of(1998, 3, 22))
        assert(last.charSpan === Some((3926,3932)))
        assert(!before.isDefined)
        assert(before.charSpan === Some((3750,3925)))
        assert(!event.isDefined)
        assert(event.charSpan === Some((3750,3759)))
        assert(!intersection.isDefined)
        assert(intersection.charSpan === Some((3750, 3932)))
      case other => fail(f"expected Seq(sunday: RI, last: I, before: I, event: I, intersection: I), found $other")
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
      </data>
    implicit val data: Data = Data(xml, None)
    val dct = SimpleInterval.of(1998, 2, 19)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(january: RepeatingField, year: RepeatingUnit, thisRI: ThisRI, intersection: Intervals) =>
        assert(january.following(dct.end).next === SimpleInterval.of(1999, 1))
        assert(january.charSpan === Some((1164,1171)))
        assert(year.following(dct.end).next === SimpleInterval.of(1999))
        assert(year.charSpan === Some((1177,1181)))
        assert(thisRI === SimpleInterval.of(1998))
        assert(thisRI.charSpan === Some((1172,1181)))
        assert(intersection === SimpleIntervals(Seq(SimpleInterval.of(1998, 1))))
        assert(intersection.charSpan === Some((1164, 1181)))
      case other => fail(f"expected Seq(january: RI, year: RI, this: I, intersection: Is), found $other")
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
      </data>
    implicit val data: Data = Data(xml, None)
    val dct = SimpleInterval.of(1998, 2, 6)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(quarterCentury: RepeatingInterval) =>
        assert(quarterCentury.preceding(dct.start).next === SimpleInterval(
          LocalDateTime.of(1950, 1, 1, 0, 0),
          LocalDateTime.of(1975, 1, 1, 0, 0)))
        assert(quarterCentury.charSpan === Some((1095, 1110)))
      case other => fail(f"expected Seq(quarter-century: RI), found $other")
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
      </data>
    implicit val data: Data = Data(xml, None)
    val dct = SimpleInterval.of(1989, 11, 1)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(century: RepeatingUnit, nth: NthRI) =>
        assert(century.following(dct.end).next === SimpleInterval(
          LocalDateTime.of(2000, 1, 1, 0, 0),
          LocalDateTime.of(2100, 1, 1, 0, 0)))
        assert(century.charSpan === Some((489,496)))
        assert(nth === SimpleInterval(
          LocalDateTime.of(1900, 1, 1, 0, 0),
          LocalDateTime.of(2000, 1, 1, 0, 0)))
        assert(nth.charSpan === Some((484, 496)))
      case other => fail(f"expected Seq(century: RI, 20th: I), found $other")
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
      </data>
    implicit val data: Data = Data(xml, None)
    val dct = SimpleInterval.of(1989, 11, 1)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(quarter: RepeatingUnit, third: NthRI) =>
        assert(quarter.following(dct.end).next === SimpleInterval(
          LocalDateTime.of(1990, 1, 1, 0, 0),
          LocalDateTime.of(1990, 4, 1, 0, 0)))
        assert(quarter.charSpan === Some((293,300)))
        assert(third === SimpleInterval(
          LocalDateTime.of(1989, 7, 1, 0, 0),
          LocalDateTime.of(1989, 10, 1, 0, 0)))
        assert(third.charSpan === Some((287, 300)))
      case other => fail(f"expected Seq(quarter: RI, third: I), found $other")
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
      </data>
    implicit val data: Data = Data(xml, None)
    val dct = SimpleInterval.of(2010, 8, 5)
    data.entities.map(new AnaforaReader(dct).temporal) match {
      case Seq(year: YearSuffix) =>
        assert(year === SimpleInterval.of(2000))
        assert(year.charSpan === Some((6422,6424)))
      case other => fail(f"expected Seq(year: YearSuffix), found $other")
    }
  }
}
