package org.clulab.timenorm

import java.time.temporal.ChronoUnit._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class TemporalTest extends FunSuite {
  
  test("TimeSpan century") {
    assertTimeSpanFromValue("20", "2000-01-01T00:00Z", "2100-01-01T00:00Z")
  }
  
  test("TimeSpan decade") {
    assertTimeSpanFromValue("189", "1890-01-01T00:00Z", "1900-01-01T00:00Z")
  }
  
  test("TimeSpan year") {
    assertTimeSpanFromValue("1999", "1999-01-01T00:00Z", "2000-01-01T00:00Z")
  }
  
  test("TimeSpan year-month") {
    assertTimeSpanFromValue("2002-11", "2002-11-01T00:00Z", "2002-12-01T00:00Z")
  }
  
  test("TimeSpan year-month-day") {
    assertTimeSpanFromValue("1980-04-25", "1980-04-25T00:00Z", "1980-04-26T00:00Z")
  }
  
  test("TimeSpan year-month-day-hour") {
    assertTimeSpanFromValue("3124-07-29T20", "3124-07-29T20:00Z", "3124-07-29T21:00Z")
  }
  
  test("TimeSpan year-month-day-hour-minute") {
    assertTimeSpanFromValue("2013-04-04T21:06", "2013-04-04T21:06Z", "2013-04-04T21:07Z")
  }
  
  test("TimeSpan year-month-day-hour-minute-second") {
    assertTimeSpanFromValue("1234-05-06T23:59:59", "1234-05-06T23:59:59Z", "1234-05-07T00:00Z")
  }
  
  test("TimeSpan year-month-day-partofday") {
    assertTimeSpanFromValue("1888-08-08TMO", "1888-08-08T00:00Z", "1888-08-08T12:00Z")
    assertTimeSpanFromValue("1888-08-08TAF", "1888-08-08T12:00Z", "1888-08-08T18:00Z")
    assertTimeSpanFromValue("1888-08-08TEV", "1888-08-08T17:00Z", "1888-08-09T00:00Z")
    assertTimeSpanFromValue("1888-08-08TNI", "1888-08-08T21:00Z", "1888-08-09T04:00Z")
  }
  
  test("TimeSpan year-season") {
    assertTimeSpanFromValue("1979-SP", "1979-03-20T00:00Z", "1979-06-21T00:00Z")
    assertTimeSpanFromValue("1979-SU", "1979-06-21T00:00Z", "1979-09-22T00:00Z")
    assertTimeSpanFromValue("1979-FA", "1979-09-22T00:00Z", "1979-12-21T00:00Z")
    assertTimeSpanFromValue("1979-WI", "1979-12-21T00:00Z", "1980-03-20T00:00Z")
  }
  
  test("TimeSpan year-quarter") {
    assertTimeSpanFromValue("1988-Q2", "1988-04-01T00:00Z", "1988-07-01T00:00Z")
  }
  
  test("TimeSpan year-week") {
    assertTimeSpanFromValue("2012-W34", "2012-08-20T00:00Z", "2012-08-27T00:00Z")
  }
  
  test("TimeSpan year-week-weekend") {
    assertTimeSpanFromValue("2012-W34-WE", "2012-08-25T00:00Z", "2012-08-27T00:00Z")
  }

  test("Period with zeroes") {
    val period = Period(Map(DAYS -> 0, HOURS -> 1, MINUTES -> 0, SECONDS -> 15))
    assert(period.timeMLValue === "PT1H15S")
  }

  test("fractional Period") {
    assert(Period.fromFractional(1, 3, YEARS).timeMLValue === "P4M")
    // as per IT-TimeML in EVENTI data (https://sites.google.com/site/eventievalita2014/)
    assert(Period.fromFractional(1, 2, MONTHS).timeMLValue === "P15D")
  }
  
  def assertTimeSpanFromValue(value: String, start: String, end: String) = {
    val timeSpan = TimeSpan.fromTimeMLValue(value)
    assert(timeSpan.start.toString === start)
    assert(timeSpan.end.toString === end)
  }
}