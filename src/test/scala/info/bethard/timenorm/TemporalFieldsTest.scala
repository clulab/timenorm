package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.LocalDate
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.DayOfWeek

@RunWith(classOf[JUnitRunner])
class TemporalFieldsTest extends FunSuite {

  test("season days") {
    assertSeasonDay(LocalDate.of(2011, 3, 19), 3, 89)
    assertSeasonDay(LocalDate.of(2011, 3, 20), 0, 1)
    assertSeasonDay(LocalDate.of(2011, 3, 21), 0, 2)
    assertSeasonDay(LocalDate.of(2011, 6, 20), 0, 93)
    assertSeasonDay(LocalDate.of(2011, 6, 21), 1, 1)
    assertSeasonDay(LocalDate.of(2011, 6, 22), 1, 2)
    assertSeasonDay(LocalDate.of(2011, 9, 21), 1, 93)
    assertSeasonDay(LocalDate.of(2011, 9, 22), 2, 1)
    assertSeasonDay(LocalDate.of(2011, 9, 23), 2, 2)
    assertSeasonDay(LocalDate.of(2011, 12, 20), 2, 90)
    assertSeasonDay(LocalDate.of(2011, 12, 21), 3, 1)
    assertSeasonDay(LocalDate.of(2011, 12, 22), 3, 2)
  }
  
  test("season days with leap year") {
    assertSeasonDay(LocalDate.of(2013, 3, 19), 3, 89)
    assertSeasonDay(LocalDate.of(2013, 3, 20), 0, 1)
    assertSeasonDay(LocalDate.of(2013, 3, 21), 0, 2)
    assertSeasonDay(LocalDate.of(2013, 6, 20), 0, 93)
    assertSeasonDay(LocalDate.of(2013, 6, 21), 1, 1)
    assertSeasonDay(LocalDate.of(2013, 6, 22), 1, 2)
    assertSeasonDay(LocalDate.of(2013, 9, 21), 1, 93)
    assertSeasonDay(LocalDate.of(2013, 9, 22), 2, 1)
    assertSeasonDay(LocalDate.of(2013, 9, 23), 2, 2)
    assertSeasonDay(LocalDate.of(2013, 12, 20), 2, 90)
    assertSeasonDay(LocalDate.of(2013, 12, 21), 3, 1)
    assertSeasonDay(LocalDate.of(2013, 12, 22), 3, 2)
  }
  
  test("setting day of season") {
    assert(LocalDate.of(2013, 3, 19).`with`(DAY_OF_SEASON, 1) === LocalDate.of(2012, 12, 21))
    assert(LocalDate.of(2013, 3, 21).`with`(DAY_OF_SEASON, 1) === LocalDate.of(2013, 3, 20))
    assert(LocalDate.of(2013, 6, 20).`with`(DAY_OF_SEASON, 1) === LocalDate.of(2013, 3, 20))
    assert(LocalDate.of(2013, 6, 22).`with`(DAY_OF_SEASON, 1) === LocalDate.of(2013, 6, 21))
    assert(LocalDate.of(2013, 9, 21).`with`(DAY_OF_SEASON, 1) === LocalDate.of(2013, 6, 21))
    assert(LocalDate.of(2013, 9, 23).`with`(DAY_OF_SEASON, 1) === LocalDate.of(2013, 9, 22))
    assert(LocalDate.of(2013, 12, 20).`with`(DAY_OF_SEASON, 1) === LocalDate.of(2013, 9, 22))
    assert(LocalDate.of(2013, 12, 22).`with`(DAY_OF_SEASON, 1) === LocalDate.of(2013, 12, 21))
  }
  
  test("adding seasons") {
    assert(LocalDate.of(2013, 3, 19).plus(1, SEASONS) === LocalDate.of(2013, 6, 16))
    assert(LocalDate.of(2013, 3, 20).minus(3, SEASONS) === LocalDate.of(2012, 6, 21))
    assert(LocalDate.of(2013, 9, 23).plus(4, SEASONS) === LocalDate.of(2014, 9, 23))
    assert(LocalDate.of(2012, 7, 14).minus(8, SEASONS) === LocalDate.of(2010, 7, 14))
    assert(LocalDate.of(2011, 12, 25).plus(7, SEASONS) === LocalDate.of(2013, 9, 26))
  }
  
  private def assertSeasonDay(date: LocalDate, seasonOfYear: Int, dayOfSeason: Int): Unit = {
    assert(date.get(SEASON_OF_YEAR) === seasonOfYear)
    assert(date.get(DAY_OF_SEASON) === dayOfSeason)
  }
}
