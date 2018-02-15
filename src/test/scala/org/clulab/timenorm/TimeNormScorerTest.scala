package org.clulab.timenorm.formal

import java.time.temporal.{ChronoField, ChronoUnit}

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.clulab.timenorm.TimeNormScorer.{parseDCT, score, intervalPrecision, intervalScores, Timex}
import org.clulab.timenorm.field.NIGHT_OF_DAY


@RunWith(classOf[JUnitRunner])
class TimeNormScorerTest extends FunSuite with TypesSuite {

  def assertScore(timex1: TimeExpression,
                  timex2: TimeExpression,
                  precision: Double,
                  recall: Double): org.scalatest.Assertion = {
    assert(score(timex2, timex1) === (recall, precision) && score(timex1, timex2) === (precision, recall))
  }

  test("parseDCT") {
    assert(parseDCT("1998-05-31") === SimpleInterval.of(1998, 5, 31))
    assert(parseDCT("2017-08-01T12") === SimpleInterval.of(2017, 8, 1, 12))
    assert(parseDCT("2017-08-01T12:59") === SimpleInterval.of(2017, 8, 1, 12, 59))
    assert(parseDCT("2017-08-01T12:59:33") === SimpleInterval.of(2017, 8, 1, 12, 59, 33))
  }

  test("NYT19980206.0460 (2979,3004) first nine months of 1997") {
    val gold = NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 9)

    // first nine months of 1997
    val timex1 = NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 9)
    assertScore(gold, timex1, 1.0, 1.0)

    // first month of 1997
    val timex2 = NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 1)
    assertScore(gold, timex2, 1.0, 31.0/273) // January has 31 days and the interval has 273 days

    // first eleven months of 1997
    val timex3 = NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 11)
    assertScore(gold, timex3, 273.0/334, 1.0) // December has 31 days (365-31=334)
  }


  test("APW19990206.0090 (767,781) Thursday night") {
    val dct = SimpleInterval.of(1999, 2, 6, 6, 22, 26)
    val thursday = RepeatingField(ChronoField.DAY_OF_WEEK, 4)
    val night = RepeatingField(NIGHT_OF_DAY, 1)
    val gold = LastRI(dct, IntersectionRI(Set(thursday, night)))

    // Thursday night
    val timex1 = LastRI(dct, IntersectionRI(Set(thursday, night)))
    assertScore(gold, timex1, 1.0, 1.0)

    // Thursday
    val timex2 = LastRI(dct, thursday)
    assertScore(gold, timex2, 3.0/24, 3.0/7) // The interval has 7 hours. Only 3 are from Thursday
  }



  test("wsj_0124 (450,457) Nov. 13") {
    val nov = RepeatingField(ChronoField.MONTH_OF_YEAR, 11)
    val nov13 = IntersectionRI(
      Set(nov, RepeatingField(ChronoField.DAY_OF_MONTH, 13)))
    val gold = LastRI(SimpleInterval.of(1989, 11, 14), nov13)


    // Nov. 13
    val timex1 = LastRI(SimpleInterval.of(1989, 11, 14), nov13)
    assertScore(gold, timex1, 1.0, 1.0)

    // Nov.
    val timex2 = LastRI(SimpleInterval.of(1989, 12), nov)
    assertScore(gold, timex2, 1.0/30, 1.0) // November has 30 days
  }

  test("intervalPrecision") {
    val jan1999ThroughDec1999 = SimpleInterval.of(1999)
    val feb1999ThroughJul1999 = NextP(SimpleInterval.of(1999, 1), SimplePeriod(ChronoUnit.MONTHS, 6))
    val aug1999ThroughFeb2000 = NextP(SimpleInterval.of(1999, 7), SimplePeriod(ChronoUnit.MONTHS, 7))

    assert(
      intervalPrecision(Seq(jan1999ThroughDec1999), Seq(feb1999ThroughJul1999, aug1999ThroughFeb2000))
        === (365 - 31) / (365.0 + 29))
    assert(
      intervalPrecision(Seq(feb1999ThroughJul1999, aug1999ThroughFeb2000), Seq(jan1999ThroughDec1999))
        === (365 - 31) / 365.0)
  }

  test("intervalScores") {
    val goldTimexes = Seq(
      Timex("G1", (0, 3), SimpleInterval.of(1980, 5)),
      Timex("G2", (5, 10), SimpleInterval.of(1985, 2, 1)),
      Timex("G3", (11, 12), SimpleInterval.of(1985, 2, 2)),
    )
    val systemTimexes = Seq(
      Timex("S1", (0, 1), SimpleInterval.of(1980, 5, 1)),
      Timex("S2", (2, 3), SimpleInterval.of(1980, 5, 31)),
      Timex("S3", (3, 5), SimpleInterval.of(1980, 5)),
      Timex("S4", (9, 12), SimpleInterval.of(1985, 2)),
    )
    val (precisions, recalls) = intervalScores(goldTimexes, systemTimexes)
    assert(precisions === Seq(1.0, 1.0, 0.0, 2/28.0))
    assert(recalls === Seq(2/31.0, 1.0, 1.0))
  }
}