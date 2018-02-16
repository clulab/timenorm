package org.clulab.timenorm.field

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.time.ZoneId

@RunWith(classOf[JUnitRunner])
class TemporalFieldsTest extends FunSuite {
  
  test("getting parts of day") {
    val apr28 = ZonedDateTime.of(LocalDateTime.of(2000, 3, 27, 3, 0), ZoneId.of("Z"))
    assert(apr28.get(NIGHT_OF_DAY) === 1)
  }
  
  test("getting week days and weekends") {
    assert(LocalDate.of(2012, 12, 9).get(WEEKEND_OF_WEEK) === 1) // Sun
    assert(LocalDate.of(2012, 12, 10).get(WEEKEND_OF_WEEK) === 0) // Mon
    assert(LocalDate.of(2012, 12, 11).get(WEEKEND_OF_WEEK) === 0) // Tue
    assert(LocalDate.of(2012, 12, 12).get(WEEKEND_OF_WEEK) === 0) // Wed
    assert(LocalDate.of(2012, 12, 13).get(WEEKEND_OF_WEEK) === 0) // Thu
    assert(LocalDate.of(2012, 12, 14).get(WEEKEND_OF_WEEK) === 0) // Fri
    assert(LocalDate.of(2012, 12, 15).get(WEEKEND_OF_WEEK) === 1) // Sat
    assert(LocalDate.of(2012, 12, 16).get(WEEKEND_OF_WEEK) === 1) // Sun
  }

  test("setting week days and weekends") {
    // only test round-tripping, since exact date of year for a "weekend" is not defined
    assert(LocalDate.of(2012, 12, 1).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 2).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 3).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 4).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 5).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 6).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 7).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 8).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 9).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
    assert(LocalDate.of(2012, 12, 10).`with`(WEEKEND_OF_WEEK, 1).get(WEEKEND_OF_WEEK) === 1)
  }

  test("adding weekends") {
    // very simple tests here because adding of "weekends" is not well defined
    val dec10 = LocalDate.of(2012, 12, 10)
    // plus makes the date later
    assert(dec10.plus(1, WEEKENDS).isAfter(dec10))
    // minus makes the date earlier
    assert(dec10.minus(1, WEEKENDS).isBefore(dec10))
    // plus a greater amount makes the date later
    assert(dec10.plus(2, WEEKENDS).isAfter(dec10.plus(1, WEEKENDS)))
  }

  test("season days") {
    assertSeasonDay(LocalDate.of(2011, 3, 19), WINTER_OF_YEAR, 89)
    assertSeasonDay(LocalDate.of(2011, 3, 20), SPRING_OF_YEAR, 1)
    assertSeasonDay(LocalDate.of(2011, 3, 21), SPRING_OF_YEAR, 2)
    assertSeasonDay(LocalDate.of(2011, 6, 20), SPRING_OF_YEAR, 93)
    assertSeasonDay(LocalDate.of(2011, 6, 21), SUMMER_OF_YEAR, 1)
    assertSeasonDay(LocalDate.of(2011, 6, 22), SUMMER_OF_YEAR, 2)
    assertSeasonDay(LocalDate.of(2011, 9, 21), SUMMER_OF_YEAR, 93)
    assertSeasonDay(LocalDate.of(2011, 9, 22), FALL_OF_YEAR, 1)
    assertSeasonDay(LocalDate.of(2011, 9, 23), FALL_OF_YEAR, 2)
    assertSeasonDay(LocalDate.of(2011, 12, 20), FALL_OF_YEAR, 90)
    assertSeasonDay(LocalDate.of(2011, 12, 21), WINTER_OF_YEAR, 1)
    assertSeasonDay(LocalDate.of(2011, 12, 22), WINTER_OF_YEAR, 2)
  }
  
  test("season days with leap year") {
    assertSeasonDay(LocalDate.of(2012, 3, 19), WINTER_OF_YEAR, 90)
    assertSeasonDay(LocalDate.of(2012, 3, 20), SPRING_OF_YEAR, 1)
    assertSeasonDay(LocalDate.of(2012, 3, 21), SPRING_OF_YEAR, 2)
    assertSeasonDay(LocalDate.of(2012, 6, 20), SPRING_OF_YEAR, 93)
    assertSeasonDay(LocalDate.of(2012, 6, 21), SUMMER_OF_YEAR, 1)
    assertSeasonDay(LocalDate.of(2012, 6, 22), SUMMER_OF_YEAR, 2)
    assertSeasonDay(LocalDate.of(2012, 9, 21), SUMMER_OF_YEAR, 93)
    assertSeasonDay(LocalDate.of(2012, 9, 22), FALL_OF_YEAR, 1)
    assertSeasonDay(LocalDate.of(2012, 9, 23), FALL_OF_YEAR, 2)
    assertSeasonDay(LocalDate.of(2012, 12, 20), FALL_OF_YEAR, 90)
    assertSeasonDay(LocalDate.of(2012, 12, 21), WINTER_OF_YEAR, 1)
    assertSeasonDay(LocalDate.of(2012, 12, 22), WINTER_OF_YEAR, 2)
  }
  
  test("setting day of season") {
    assert(LocalDate.of(2013, 2, 17).`with`(DAY_OF_SPRING, 1) === LocalDate.of(2013, 3, 20))
    assert(LocalDate.of(2013, 3, 16).`with`(DAY_OF_SPRING, 93) === LocalDate.of(2013, 6, 20))
    assert(LocalDate.of(2013, 4, 15).`with`(DAY_OF_SUMMER, 1) === LocalDate.of(2013, 6, 21))
    assert(LocalDate.of(2013, 5, 14).`with`(DAY_OF_SUMMER, 93) === LocalDate.of(2013, 9, 21))
    assert(LocalDate.of(2013, 6, 13).`with`(DAY_OF_FALL, 1) === LocalDate.of(2013, 9, 22))
    assert(LocalDate.of(2013, 7, 12).`with`(DAY_OF_FALL, 90) === LocalDate.of(2013, 12, 20))
    assert(LocalDate.of(2013, 8, 11).`with`(DAY_OF_WINTER, 1) === LocalDate.of(2013, 12, 21))
    assert(LocalDate.of(2013, 9, 10).`with`(DAY_OF_WINTER, 89) === LocalDate.of(2013, 3, 19))
  }
  
  test("adding seasons") {
    // very simple tests here because adding of seasons is not well defined
    val dec10 = LocalDate.of(2012, 12, 10)
    val test = (seasons: PartialRange) => {
      // plus makes the date later
      assert(dec10.plus(1, seasons).isAfter(dec10))
      // minus makes the date earlier
      assert(dec10.minus(1, seasons).isBefore(dec10))
      // plus a greater amount makes the date later
      assert(dec10.plus(2, seasons).isAfter(dec10.plus(1, seasons)))
    }
    test(SPRINGS)
    test(SUMMERS)
    test(FALLS)
    test(WINTERS)
  }

  test("quarter centuries") {
    assert(LocalDate.of(2017, 8, 9).plus(2L, QUARTER_CENTURIES) === LocalDate.of(2067, 8, 9))
  }
  
  private def assertSeasonDay(date: LocalDate, seasonOfYear: PartialOfRangeUnit, dayOfSeason: Int): Unit = {
    assert(date.get(seasonOfYear) === 1)
    for (otherSeason <- Set(SPRING_OF_YEAR, SUMMER_OF_YEAR, FALL_OF_YEAR, WINTER_OF_YEAR) - seasonOfYear) {
      assert(date.get(otherSeason) == 0)
    }
    val ofYearToDayOf = Map(
        SPRING_OF_YEAR -> DAY_OF_SPRING,
        SUMMER_OF_YEAR -> DAY_OF_SUMMER,
        FALL_OF_YEAR -> DAY_OF_FALL,
        WINTER_OF_YEAR -> DAY_OF_WINTER) 
    assert(date.get(ofYearToDayOf(seasonOfYear)) === dayOfSeason)
  }
}
