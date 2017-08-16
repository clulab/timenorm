package info.bethard.timenorm.formal

import java.time.temporal.{ChronoField, ChronoUnit}

import info.bethard.anafora.Data
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import info.bethard.timenorm.TimeNormScorer.{get_intervals, score, parseDCT}
import info.bethard.timenorm.field.{NIGHT_OF_DAY}


@RunWith(classOf[JUnitRunner])
class TimeNormScorerTest extends FunSuite with TypesSuite {

  test("parseDCT") {
    assert(parseDCT("1998-05-31") === SimpleInterval.of(1998, 5, 31))
    assert(parseDCT("2017-08-01T12") === SimpleInterval.of(2017, 8, 1, 12))
    assert(parseDCT("2017-08-01T12:59") === SimpleInterval.of(2017, 8, 1, 12, 59))
    assert(parseDCT("2017-08-01T12:59:33") === SimpleInterval.of(2017, 8, 1, 12, 59, 33))
  }

  test("NYT19980206.0460 (2979,3004) first nine months of 1997") {
    val gold = NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 9)
    val gold_intervals = get_intervals(gold)

    // first nine months of 1997
    val timex1 = NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 9)
    val timex1_intervals = get_intervals(timex1)
    assert(
      score(gold, gold_intervals, timex1, timex1_intervals)  === (1.0, 1.0)
    )

    // first month of 1997
    val timex2 = NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 1)
    val timex2_intervals = get_intervals(timex2)
    assert(
      score(gold, gold_intervals, timex2, timex2_intervals)  === (1.0, (31.0/273)) // January has 31 days and the interval has 273 days
    )

    // first eleven months of 1997
    val timex3 = NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 11)
    val timex3_intervals = get_intervals(timex3)
    assert(
      score(gold, gold_intervals, timex3, timex3_intervals)  === ((273.0/334), 1.0) // December has 31 days (365-31=334)
    )
  }


  test("APW19990206.0090 (767,781) Thursday night") {
    val dct = SimpleInterval.of(1999, 2, 6, 6, 22, 26)
    val thursday = RepeatingField(ChronoField.DAY_OF_WEEK, 4)
    val night = RepeatingField(NIGHT_OF_DAY, 1)
    val gold = LastRI(dct, IntersectionRI(Set(thursday, night)))
    val gold_intervals = get_intervals(gold)

    // Thursday night
    val timex1 = LastRI(dct, IntersectionRI(Set(thursday, night)))
    val timex1_intervals = get_intervals(timex1)
    assert(
      score(gold, gold_intervals, timex1, timex1_intervals)  === (1.0, 1.0)
    )

    // Thursday
    val timex2 = LastRI(dct, thursday)
    val timex2_intervals = get_intervals(timex2)
    assert(
      score(gold, gold_intervals, timex2, timex2_intervals)  === ((3.0/24), (3.0/7)) // The interval has 7 hours. Only 3 are from Thursday
    )
  }



  test("wsj_0124 (450,457) Nov. 13") {
    val nov = RepeatingField(ChronoField.MONTH_OF_YEAR, 11)
    val nov13 = IntersectionRI(
      Set(nov, RepeatingField(ChronoField.DAY_OF_MONTH, 13)))
    val gold = LastRI(SimpleInterval.of(1989, 11, 14), nov13)
    val gold_intervals = get_intervals(gold)


    // Nov. 13
    val timex1 = LastRI(SimpleInterval.of(1989, 11, 14), nov13)
    val timex1_intervals = get_intervals(timex1)
    assert(
      score(gold, gold_intervals, timex1, timex1_intervals) === (1.0, 1.0)
    )

    // Nov.
    val timex2 = LastRI(SimpleInterval.of(1989, 12), nov)
    val timex2_intervals = get_intervals(timex2)
    assert(
      score(gold, gold_intervals, timex2, timex2_intervals) === ((1.0/30), 1.0) // November has 30 days
    )
  }
}