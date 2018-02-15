package org.clulab.timenorm.formal

import java.time.temporal._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.{DateTimeException, DayOfWeek, LocalDateTime}
import java.util.Collections.singletonList

import org.clulab.timenorm.field.{NIGHT_OF_DAY, SUMMER_OF_YEAR, WINTER_OF_YEAR}
import org.scalactic.Prettifier

trait TypesSuite {
  implicit def intervalEquality[T <: Interval] = new org.scalactic.Equality[T] {
    override def areEqual(a: T, b: Any): Boolean = b match {
      case Interval(bStart, bEnd) => a.start == bStart && a.end == bEnd
      case _ => false
    }
  }

  implicit def intervalsEquality[T <: Seq[_ <: Interval]] = new org.scalactic.Equality[T] {
    override def areEqual(a: T, b: Any): Boolean = b match {
      case i: Seq[_] => a.size == i.size && (a zip i).forall{ case (x, y) => intervalEquality.areEqual(x, y) }
      case _ => false
    }
  }

  implicit val prettifier = new Prettifier {
    override def apply(o: Any): String = o match {
      case interval: SimpleInterval => interval.toString
      case interval: Interval => s"$interval with range [${interval.start}, ${interval.end})"
      case seq: Seq[_] =>
        val intervals = seq.collect{ case Interval(s, e) => (s, e) }
        if (intervals.isEmpty) Prettifier.default(seq)
        else s"""$seq with ranges ${intervals.mkString("[", ",", "]")}"""
      case _ => Prettifier.default(o)
    }
  }
}

@RunWith(classOf[JUnitRunner])
class TypesTest extends FunSuite with TypesSuite {

  test("SimpleInterval") {
    val year = SimpleInterval.of(1985)
    assert(year.start === LocalDateTime.of(1985, 1, 1, 0, 0, 0, 0))
    assert(year.end === LocalDateTime.of(1986, 1, 1, 0, 0, 0, 0))

    val yearMonth = SimpleInterval.of(1985, 6)
    assert(yearMonth.start === LocalDateTime.of(1985, 6, 1, 0, 0, 0, 0))
    assert(yearMonth.end === LocalDateTime.of(1985, 7, 1, 0, 0, 0, 0))

    val yearMonthDay = SimpleInterval.of(1985, 6, 17)
    assert(yearMonthDay.start === LocalDateTime.of(1985, 6, 17, 0, 0, 0, 0))
    assert(yearMonthDay.end === LocalDateTime.of(1985, 6, 18, 0, 0, 0, 0))

    val yearMonthDayHour = SimpleInterval.of(1985, 6, 17, 23)
    assert(yearMonthDayHour.start === LocalDateTime.of(1985, 6, 17, 23, 0, 0, 0))
    assert(yearMonthDayHour.end === LocalDateTime.of(1985, 6, 18, 0, 0, 0, 0))

    val yearMonthDayHourMinute = SimpleInterval.of(1985, 6, 17, 23, 0)
    assert(yearMonthDayHourMinute.start === LocalDateTime.of(1985, 6, 17, 23, 0, 0, 0))
    assert(yearMonthDayHourMinute.end === LocalDateTime.of(1985, 6, 17, 23, 1, 0, 0))
  }

  test("Year") {
    val year = Year(1985)
    assert(year.start === LocalDateTime.of(1985, 1, 1, 0, 0, 0, 0))
    assert(year.end === LocalDateTime.of(1986, 1, 1, 0, 0, 0, 0))

    val decade = Year(198, 1)
    assert(decade.start === LocalDateTime.of(1980, 1, 1, 0, 0, 0, 0))
    assert(decade.end === LocalDateTime.of(1990, 1, 1, 0, 0, 0, 0))

    val century = Year(17, 2)
    assert(century.start === LocalDateTime.of(1700, 1, 1, 0, 0, 0, 0))
    assert(century.end === LocalDateTime.of(1800, 1, 1, 0, 0, 0, 0))
  }

  test("YearSuffix") {
    assert(YearSuffix(Year(1903), 37, 2) === SimpleInterval.of(1937))
    assert(YearSuffix(Year(2016), 418, 3) === SimpleInterval.of(2418))
    assert(YearSuffix(Year(132, 1), 85, 2) === SimpleInterval.of(1385))
    assert(YearSuffix(Year(23, 2), 22, 2) === SimpleInterval.of(2322))

    val yearPlus1DigitDecade = YearSuffix(Year(1903), 3, 1, 1)
    assert(yearPlus1DigitDecade.start === LocalDateTime.of(1930, 1, 1, 0, 0, 0, 0))
    assert(yearPlus1DigitDecade.end === LocalDateTime.of(1940, 1, 1, 0, 0, 0, 0))

    val decadePlus1DigitDecade = YearSuffix(Year(132, 1), 8, 1, 1)
    assert(decadePlus1DigitDecade.start === LocalDateTime.of(1380, 1, 1, 0, 0, 0, 0))
    assert(decadePlus1DigitDecade.end === LocalDateTime.of(1390, 1, 1, 0, 0, 0, 0))

    val decadePlus3DigitDecade = YearSuffix(Year(132, 1), 240, 3, 1)
    assert(decadePlus3DigitDecade.start === LocalDateTime.of(2400, 1, 1, 0, 0, 0, 0))
    assert(decadePlus3DigitDecade.end === LocalDateTime.of(2410, 1, 1, 0, 0, 0, 0))
  }

  test("SimplePeriod") {
    val ldt = LocalDateTime.of(2000, 1, 1, 0, 0, 0, 0)
    val number = IntNumber(5)
    val unit = ChronoUnit.YEARS
    val mod = Modifier.Exact

    val simple = SimplePeriod(unit, number, mod)
    assert(simple.addTo(ldt) === LocalDateTime.of(2005, 1, 1, 0, 0, 0, 0))
    assert(simple.subtractFrom(ldt) === LocalDateTime.of(1995, 1, 1, 0, 0, 0, 0))
    assert(simple.get(unit) === 5)
    assert(simple.getUnits() === singletonList(unit))

    //Expected failures to follow
    intercept[UnsupportedTemporalTypeException] {
      assert(simple.get(ChronoUnit.MONTHS) === 60)
    }

    val vagueNumber = VagueNumber("A few")

    intercept[scala.MatchError] {
      SimplePeriod(unit, vagueNumber, mod).addTo(ldt)
    }
  }

  test("SumP") {
    val period1 = SimplePeriod(ChronoUnit.YEARS, 1, Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, 2, Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, 3, Modifier.Approx)
    val period4 = SimplePeriod(ChronoUnit.DAYS, 2, Modifier.Mid)
    val periodSum = SumP(Set(period1, period2, period3), Modifier.Exact)
    val ldt = LocalDateTime.of(2000, 6, 10, 0, 0, 0, 0)

    val list = new java.util.ArrayList[TemporalUnit]
    list.add(ChronoUnit.YEARS)
    list.add(ChronoUnit.MONTHS)

    assert(periodSum.addTo(ldt) === LocalDateTime.of(2003, 9, 10, 0, 0, 0, 0))
    assert(periodSum.subtractFrom(ldt) === LocalDateTime.of(1997, 3, 10, 0, 0, 0, 0))
    assert(periodSum.get(ChronoUnit.YEARS) == 3)
    assert(periodSum.getUnits() === list)

    //Tests for periodSums that contain periodSums
    val periodSum2 = SumP(Set(period4, periodSum), Modifier.Fiscal)
    list.add(ChronoUnit.DAYS)

    assert(periodSum2.addTo(ldt) === LocalDateTime.of(2003, 9, 12, 0, 0, 0, 0))
    assert(periodSum2.subtractFrom(ldt) === LocalDateTime.of(1997, 3, 8, 0, 0, 0, 0))
    assert(periodSum2.get(ChronoUnit.DAYS) == 2)
    assert(periodSum2.getUnits() === list)

    intercept[UnsupportedTemporalTypeException] {
      periodSum2.get(ChronoUnit.HOURS)
    }
  }

  test("LastP") {
    val period1 = SimplePeriod(ChronoUnit.YEARS, 1, Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, 2, Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, 3, Modifier.Approx)
    val periodSum = SumP(Set(period1, period2, period3), Modifier.Exact)

    val year = Year(2000)
    val lastPeriod = LastP(year, period1)

    assert(lastPeriod.end === year.start)
    assert(lastPeriod.start === LocalDateTime.of(1999, 1, 1, 0, 0, 0, 0))

    val lastPeriod1 = LastP(year, periodSum)

    assert(lastPeriod1.end === year.start)
    assert(lastPeriod1.start === LocalDateTime.of(1996, 10, 1, 0, 0, 0, 0))
  }

  test("NextP") {
    assert(NextP(Year(2000), SimplePeriod(ChronoUnit.YEARS, 1)) === SimpleInterval.of(2001))

    assert(NextP(SimpleInterval.of(2017, 8, 16), SimplePeriod(ChronoUnit.WEEKS, 2))
      === SimpleInterval(LocalDateTime.of(2017, 8, 17, 0, 0), LocalDateTime.of(2017, 8, 31, 0, 0)))

    assert(NextP(Year(2000), SumP(Set(
      SimplePeriod(ChronoUnit.YEARS, 1, Modifier.Exact),
      SimplePeriod(ChronoUnit.YEARS, 2, Modifier.Fiscal),
      SimplePeriod(ChronoUnit.MONTHS, 3, Modifier.Approx))))
      === SimpleInterval(LocalDateTime.of(2001, 1, 1, 0, 0), LocalDateTime.of(2004, 4, 1, 0, 0, 0, 0)))
  }

  test("BeforeP") {
    val period1 = SimplePeriod(ChronoUnit.YEARS, 1, Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, 2, Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, 3, Modifier.Approx)
    val periodSum = SumP(Set(period1, period2, period3), Modifier.Exact)

    val year = Year(2000)
    val beforePeriod = BeforeP(year, period1)

    assert(beforePeriod.start === LocalDateTime.of(1999, 1, 1, 0, 0, 0, 0))
    assert(beforePeriod.end === LocalDateTime.of(2000, 1, 1, 0, 0, 0, 0))

    val beforePeriod1 = BeforeP(year, periodSum)

    assert(beforePeriod1.start === LocalDateTime.of(1996, 10, 1, 0, 0, 0, 0))
    assert(beforePeriod1.end === LocalDateTime.of(1997, 10, 1, 0, 0, 0, 0))

    // 2 weeks Before July 28 is the 7-day interval around July 14
    assert(
      BeforeP(SimpleInterval.of(2017, 7, 28), SimplePeriod(ChronoUnit.WEEKS, 2))
        === SimpleInterval(LocalDateTime.of(2017, 7, 11, 0, 0), LocalDateTime.of(2017, 7, 18, 0, 0)))
  }

  test("AfterP") {
    val period1 = SimplePeriod(ChronoUnit.YEARS, 1, Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, 2, Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, 3, Modifier.Approx)
    val periodSum = SumP(Set(period1, period2, period3), Modifier.Exact)

    val year = Year(2000)
    val afterPeriod = AfterP(year, period1)

    assert(afterPeriod.start === LocalDateTime.of(2001, 1, 1, 0, 0, 0, 0))
    assert(afterPeriod.end === LocalDateTime.of(2002, 1, 1, 0, 0, 0, 0))

    val afterPeriod1 = AfterP(year, periodSum)

    assert(afterPeriod1.start === LocalDateTime.of(2003, 4, 1, 0, 0, 0, 0))
    assert(afterPeriod1.end === LocalDateTime.of(2004, 4, 1, 0, 0, 0, 0))

    // 3 months After January 25 is the 1-month interval around April 25
    assert(
      AfterP(SimpleInterval.of(2000, 1, 25), SimplePeriod(ChronoUnit.MONTHS, 3))
        === SimpleInterval(LocalDateTime.of(2000, 4, 10, 0, 0), LocalDateTime.of(2000, 5, 10, 0, 0)))
  }

  test("ThisP") {
    val period1 = SimplePeriod(ChronoUnit.YEARS, 1, Modifier.Exact)

    val year = Year(2002)
    val thisPeriod = ThisP(year, period1)

    assert(thisPeriod.start === LocalDateTime.of(2002, 1, 1, 0, 0, 0, 0))
    assert(thisPeriod.end === LocalDateTime.of(2003, 1, 1, 0, 0, 0, 0))

    val interval = SimpleInterval(LocalDateTime.of(2001, 1, 1, 0, 0, 0, 0), LocalDateTime.of(2001, 1, 1, 0, 0, 0, 0))
    val period = SimplePeriod(ChronoUnit.DAYS, 5, Modifier.Exact)
    val thisPeriod2 = ThisP(interval, period)

    assert(thisPeriod2.start === LocalDateTime.of(2000, 12, 29, 12, 0, 0, 0))
    assert(thisPeriod2.end === LocalDateTime.of(2001, 1, 3, 12, 0, 0, 0))
  }

  test("Between") {
    val interval1 = Year(1999)
    val interval2 = Year(2002)
    val between = Between(interval1, interval2)

    assert(between.start === LocalDateTime.of(2000, 1, 1, 0, 0, 0, 0))
    assert(between.end === LocalDateTime.of(2002, 1, 1, 0, 0, 0, 0))
  }

  test("NthP") {
    assert(NthP(Year(2001), 2, SimplePeriod(ChronoUnit.YEARS, 1)) === SimpleInterval.of(2002))
    assert(NthP(Year(2001), 2, SimplePeriod(ChronoUnit.YEARS, 1), from = Interval.End) === SimpleInterval.of(2000))

    val periodSum = SumP(Set(SimplePeriod(ChronoUnit.YEARS, 1), SimplePeriod(ChronoUnit.MINUTES, 20)))
    assert(NthP(Year(2001), 4, periodSum)
      === SimpleInterval(LocalDateTime.of(2004, 1, 1, 1, 0, 0), LocalDateTime.of(2005, 1, 1, 1, 20, 0)))
  }

  test("RepeatingUnit") {
    val ldt1 = LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0)
    val ldt2 = LocalDateTime.of(2003, 5, 11, 22, 10, 20, 0)
    val interval = SimpleInterval(ldt1, ldt2)

    val pre = RepeatingUnit(ChronoUnit.MONTHS, Modifier.Exact).preceding(interval.start)
    assert(pre.next === SimpleInterval.of(2002, 2))
    assert(pre.next === SimpleInterval.of(2002, 1))

    val follow = RepeatingUnit(ChronoUnit.MONTHS, Modifier.Exact).following(interval.end)
    assert(follow.next === SimpleInterval.of(2003, 6))
    assert(follow.next === SimpleInterval.of(2003, 7))

    //Truncate method tests
    val mod = Modifier.End
    val centuryRI = RepeatingUnit(ChronoUnit.CENTURIES, mod)
    val decadeRI = RepeatingUnit(ChronoUnit.DECADES, mod)
    val weeksRI = RepeatingUnit(ChronoUnit.WEEKS, mod)

    assert(
      centuryRI.preceding(interval.start).next
        === SimpleInterval(LocalDateTime.of(1900, 1, 1, 0, 0), LocalDateTime.of(2000, 1, 1, 0, 0)))

    assert(
      centuryRI.following(interval.end).next
        === SimpleInterval(LocalDateTime.of(2100, 1, 1, 0, 0), LocalDateTime.of(2200, 1, 1, 0, 0)))

    assert(
      decadeRI.preceding(interval.start).next
        === SimpleInterval(LocalDateTime.of(1990, 1, 1, 0, 0), LocalDateTime.of(2000, 1, 1, 0, 0)))

    assert(
      decadeRI.following(interval.end).next
        === SimpleInterval(LocalDateTime.of(2010, 1, 1, 0, 0), LocalDateTime.of(2020, 1, 1, 0, 0)))

    // March 11, 2002 is a Monday
    assert(
      weeksRI.preceding(interval.start).next
        === SimpleInterval(LocalDateTime.of(2002, 3, 11, 0, 0), LocalDateTime.of(2002, 3, 18, 0, 0)))

    // May 12, 2003 is a Monday
    assert(
      weeksRI.following(interval.end).next
        === SimpleInterval(LocalDateTime.of(2003, 5, 12, 0, 0), LocalDateTime.of(2003, 5, 19, 0, 0)))

    val interval2 = SimpleInterval(
      LocalDateTime.of(2001, 2, 12, 3, 3), LocalDateTime.of(2001, 2, 14, 22, 0))
    val daysRI = RepeatingUnit(ChronoUnit.DAYS, Modifier.Exact)
    assert(daysRI.preceding(interval2.start).next === SimpleInterval.of(2001, 2, 11))
    assert(daysRI.following(interval2.end).next === SimpleInterval.of(2001, 2, 15))

    val interval3 = SimpleInterval(
      LocalDateTime.of(2001, 2, 12, 0, 0), LocalDateTime.of(2001, 2, 14, 0, 0))
    assert(daysRI.preceding(interval3.start).next === SimpleInterval.of(2001, 2, 11))
    assert(daysRI.following(interval3.end).next === SimpleInterval.of(2001, 2, 14))

    // December 31, 2012 is a Monday
    val ldt3 = LocalDateTime.of(2013,1,8,0,0)
    assert(
      weeksRI.preceding(ldt3).next
        === SimpleInterval(LocalDateTime.of(2012, 12, 31, 0, 0), LocalDateTime.of(2013, 1, 7, 0, 0)))
  }

  test("RepeatingField") {
    val interval = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 5, 10, 22, 10, 20, 0))

    val monthMay = RepeatingField(ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact)
    val pre = monthMay.preceding(interval.start)
    assert(pre.next === SimpleInterval.of(2001, 5))
    assert(pre.next === SimpleInterval.of(2000, 5))
    val post = monthMay.following(interval.end)
    assert(post.next === SimpleInterval.of(2004, 5))
    assert(post.next === SimpleInterval.of(2005, 5))

    val day29 = RepeatingField(ChronoField.DAY_OF_MONTH, 29, Modifier.Exact)
    val pre2 = day29.preceding(interval.start)
    assert(pre2.next === SimpleInterval.of(2002, 1, 29))
    assert(pre2.next === SimpleInterval.of(2001, 12, 29))
    val post2 = day29.following(interval.end)
    assert(post2.next === SimpleInterval.of(2003, 5, 29))
    assert(post2.next === SimpleInterval.of(2003, 6, 29))

    // make sure that preceding and following are strict (no overlap allowed)
    val nov = RepeatingField(ChronoField.MONTH_OF_YEAR, 11)
    assert(nov.preceding(LocalDateTime.of(1989, 11, 2, 0, 0)).next === SimpleInterval.of(1988, 11))
    assert(nov.following(LocalDateTime.of(1989, 11, 2, 0, 0)).next === SimpleInterval.of(1990, 11))

    //No Exception at FieldRepeatingInterval instantiation
    val day300 = RepeatingField(ChronoField.DAY_OF_MONTH, 300, Modifier.Approx)
    intercept[DateTimeException] {
      //Exception thrown here
      val testException = day300.preceding(interval.start)
    }

    val day31 = RepeatingField(ChronoField.DAY_OF_MONTH, 31)
    val startOfMar1980 = LocalDateTime.of(1980, 3, 1, 0, 0)
    assert(day31.following(startOfMar1980).next === SimpleInterval.of(1980, 3, 31))

    val feb15 = LocalDateTime.of(2000, 2, 15, 0, 0)
    assert(day31.following(feb15).next === SimpleInterval.of(2000, 3, 31))
    assert(day31.preceding(feb15).next === SimpleInterval.of(2000, 1, 31))
  }

  test("LastRI") {
    val interval = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 5, 10, 22, 10, 20, 0))
    val may = RepeatingField(ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact)
    val friday = RepeatingField(ChronoField.DAY_OF_WEEK, 5)
    val day = RepeatingUnit(ChronoUnit.DAYS, Modifier.Exact)

    assert(LastRI(interval, may) === SimpleInterval.of(2001, 5))
    assert(LastRI(interval, day) === SimpleInterval.of(2002, 3, 21))
    assert(LastRI(SimpleInterval.of(2017, 7, 7), day) === SimpleInterval.of(2017, 7, 6))
    // July 7, 2017 is a Friday
    assert(LastRI(SimpleInterval.of(2017, 7, 7), friday) === SimpleInterval.of(2017, 6, 30))
    assert(LastRI(SimpleInterval.of(2017, 7, 8), friday) === SimpleInterval.of(2017, 7, 7))
    assert(LastRI(SimpleInterval.of(2017, 7, 6), friday) === SimpleInterval.of(2017, 6, 30))

    // January 2nd is the first Monday of 2017
    val lastWeek = LastRI(SimpleInterval.of(2017, 1, 9), RepeatingUnit(ChronoUnit.WEEKS))
    assert(lastWeek.start === LocalDateTime.of(2017, 1, 2, 0, 0))
    assert(lastWeek.end === LocalDateTime.of(2017, 1, 9, 0, 0))

    assert(LastRI(interval, RepeatingUnit(IsoFields.QUARTER_YEARS))
      === SimpleInterval(LocalDateTime.of(2001, 10, 1, 0, 0), LocalDateTime.of(2002, 1, 1, 0, 0)))

    assert(LastRI(SimpleInterval.of(2017, 7, 7), day, from = Interval.End) === SimpleInterval.of(2017, 7, 7))
    // July 7, 2017 is a Friday
    assert(LastRI(SimpleInterval.of(2017, 7, 7), friday, from = Interval.End) === SimpleInterval.of(2017, 7, 7))
    assert(LastRI(SimpleInterval.of(2017, 7, 8), friday, from = Interval.End) === SimpleInterval.of(2017, 7, 7))
    assert(LastRI(SimpleInterval.of(2017, 7, 6), friday, from = Interval.End) === SimpleInterval.of(2017, 6, 30))
  }

  test("LastRIs") {
    val interval = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 5, 10, 22, 10, 20, 0))
    val may = RepeatingField(ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact)
    val day = RepeatingUnit(ChronoUnit.DAYS, Modifier.Exact)

    //Interval: March 22, 2002 @ 11:30:30 to May 10, 2003 @ 22:10:20
    //FieldRI: May
    //Expected: Sequence(May 2001, May 2000, May 1999)
    assert(LastRIs(interval, may, 3) === SimpleIntervals(2001 to 1999 by -1 map (y => SimpleInterval.of(y, 5))))

    //Interval: March 22, 2002 @ 11:30:30 to May 10, 2003 @ 22:10:20
    //UnitRI: Days
    //Expected: Sequence(March 21, March 20, March 19, March 18, March 17 of 2002)
    assert(LastRIs(interval, day, 5) === SimpleIntervals(21 to 17 by -1 map (d => SimpleInterval.of(2002, 3, d))))

    assert(LastRIs(interval, day, 1, from = Interval.End) === SimpleIntervals(Seq(SimpleInterval.of(2003, 5, 9))))
  }

  test("NextRI") {
    val interval = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 5, 10, 22, 10, 20, 0))
    val may = RepeatingField(ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact)

    assert(NextRI(interval, may) === SimpleInterval.of(2004, 5))
    assert(NextRI(interval, RepeatingUnit(ChronoUnit.DAYS)) === SimpleInterval.of(2003, 5, 11))

    // January 2nd is the first Monday of 2017
    val nextWeek = NextRI(SimpleInterval.of(2017, 1, 8), RepeatingUnit(ChronoUnit.WEEKS))
    assert(nextWeek === SimpleInterval(LocalDateTime.of(2017, 1, 9, 0, 0), LocalDateTime.of(2017, 1, 16, 0, 0)))

    assert(NextRI(interval, may, from = Interval.Start) === SimpleInterval.of(2002, 5))
  }

  test("NextRIs") {
    val interval = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 5, 10, 22, 10, 20, 0))
    val may = RepeatingField(ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact)
    val day = RepeatingUnit(ChronoUnit.DAYS, Modifier.Exact)

    //Interval: March 22, 2002 @ 11:30:30 to May 10, 2003 @ 22:10:20
    //FieldRI: May
    //Expected: Sequence(May 2004, May 2005, May 2006)
    assert(NextRIs(interval, may, 3) === SimpleIntervals(2004 to 2006 map (y => SimpleInterval.of(y, 5))))

    //Interval: March 22, 2002 @ 11:30:30 to May 10, 2003 @ 22:10:20
    //UnitRI: Days
    //Expected: Sequence(May 11, May 12, May 13, May 14, May 15 of 2003)
    assert(NextRIs(interval, day, 5) === SimpleIntervals(11 to 15 map (d => SimpleInterval.of(2003, 5, d))))

    assert(
      NextRIs(interval, day, 3, from = Interval.Start)
        === SimpleIntervals(23 to 25 map (d => SimpleInterval.of(2002, 3, d))))
  }

  test("AfterRI") {
    val interval = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 5, 10, 22, 10, 20, 0))
    val may = RepeatingField(ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact)
    val day = RepeatingUnit(ChronoUnit.DAYS, Modifier.Exact)

    assert(AfterRI(interval, may) === SimpleInterval.of(2004, 5))
    assert(AfterRI(interval, may, 10) === SimpleInterval.of(2013, 5))
    assert(AfterRI(interval, day) === SimpleInterval.of(2003, 5, 11))
    assert(AfterRI(interval, day, 11) === SimpleInterval.of(2003, 5, 21))

    assert(AfterRI(interval, may, from = Interval.Start) === SimpleInterval.of(2002, 5))
    assert(AfterRI(interval, day, from = Interval.Start) === SimpleInterval.of(2002, 3, 23))
  }

  test("BeforeRI") {
    val interval = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 5, 10, 22, 10, 20, 0))
    val may = RepeatingField(ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact)
    val day = RepeatingUnit(ChronoUnit.DAYS, Modifier.Exact)

    assert(BeforeRI(interval, may) === SimpleInterval.of(2001, 5))
    assert(BeforeRI(interval, may, 5) === SimpleInterval.of(1997, 5))
    assert(BeforeRI(interval, day) === SimpleInterval.of(2002, 3, 21))
    assert(BeforeRI(interval, day, 20) === SimpleInterval.of(2002, 3, 2))

    assert(BeforeRI(interval, may, from = Interval.End) === SimpleInterval.of(2002, 5))
    assert(BeforeRI(interval, day, from = Interval.End) === SimpleInterval.of(2003, 5, 9))
  }

  test("NthRI") {
    val interval = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 5, 10, 22, 10, 20, 0))

    assert(NthRI(interval, 1, RepeatingField(ChronoField.MONTH_OF_YEAR, 5)) === SimpleInterval.of(2002, 5))
    assert(NthRI(interval, 1, RepeatingField(ChronoField.MONTH_OF_YEAR, 5), from = Interval.End)
      === SimpleInterval.of(2002, 5))

    assert(NthRI(interval, 3, RepeatingUnit(ChronoUnit.DAYS)) === SimpleInterval.of(2002, 3, 25))
    assert(NthRI(interval, 3, RepeatingUnit(ChronoUnit.DAYS), from = Interval.End) === SimpleInterval.of(2003, 5, 7))

    intercept[UnsupportedOperationException] {
      val Interval(_, _) = NthRI(interval, 5, RepeatingField(ChronoField.MONTH_OF_YEAR, 5))
    }

    intercept[UnsupportedOperationException] {
      val Interval(_, _) = NthRI(interval, 2, RepeatingField(ChronoField.MONTH_OF_YEAR, 5), from = Interval.End)
    }

    assert(NthRI(SimpleInterval.of(2017), 4, RepeatingUnit(IsoFields.QUARTER_YEARS))
      === SimpleInterval(LocalDateTime.of(2017, 10, 1, 0, 0), LocalDateTime.of(2018, 1, 1, 0, 0)))
  }

  test("ThisRI") {
    var interval: Interval = Year(2016)
    var repeatingInterval: RepeatingInterval = RepeatingUnit(ChronoUnit.DAYS)

    //Multiple element result, ThisRepeatingIntervals should be used instead
    intercept[MatchError] {
      val Interval(start, end) = ThisRI(interval, repeatingInterval)
    }

    //Interval: The Year of 2016
    //RI: April
    repeatingInterval = RepeatingField(ChronoField.MONTH_OF_YEAR, 4)
    var thisRI = ThisRI(interval, repeatingInterval)
    //Expected: "This April" April 2016
    assert((thisRI.start, thisRI.end) ===
      (LocalDateTime.of(2016, 4, 1, 0, 0), LocalDateTime.of(2016, 5, 1, 0, 0)))

    //Interval: July 1, 2016
    //RI: Months
    interval = SimpleInterval(LocalDateTime.of(2016, 7, 1, 0, 0), LocalDateTime.of(2016, 7, 2, 0, 0))
    repeatingInterval = RepeatingUnit(ChronoUnit.MONTHS)
    //Expected: "This Month" July 2016
    thisRI = ThisRI(interval, repeatingInterval)
    assert((thisRI.start, thisRI.end) ===
      (LocalDateTime.of(2016, 7, 1, 0, 0), LocalDateTime.of(2016, 8, 1, 0, 0)))

    // Interval: July 1, 2016
    // RI: Summers
    // Expected: June 21, 2016 through September 22, 2016
    thisRI = ThisRI(interval, RepeatingField(SUMMER_OF_YEAR, 1))
    assert((thisRI.start, thisRI.end) ===
      (LocalDateTime.of(2016, 6, 21, 0, 0), LocalDateTime.of(2016, 9, 22, 0, 0)))

    // Interval: July 1, 2016
    // RI: Winters
    // Expected: December 21, 2016 through March 20, 2017
    thisRI = ThisRI(interval, RepeatingField(WINTER_OF_YEAR, 1))
    assert((thisRI.start, thisRI.end) ===
      (LocalDateTime.of(2016, 12, 21, 0, 0), LocalDateTime.of(2017, 3, 20, 0, 0)))

    // Interval: July 1, 2016
    // RI: NIGHTS
    // Expected: July 1, 2016 at 21:00 through July 2, 2016 at 04:00
    thisRI = ThisRI(interval, RepeatingField(NIGHT_OF_DAY, 1))
    assert((thisRI.start, thisRI.end) ===
      (LocalDateTime.of(2016, 7, 1, 21, 0), LocalDateTime.of(2016, 7, 2, 4, 0)))
  }

  test("ThisRIs") {
    //~One day, Tuesday (1st of February 2005)
    val interval = SimpleInterval(
      LocalDateTime.of(2005, 2, 1, 3, 22), LocalDateTime.of(2005, 2, 2, 0, 0))

    //One week, Thursday the 10th through Thursday the 17th of April 2003
    val interval1 = SimpleInterval(
      LocalDateTime.of(2003, 4, 10, 0, 0), LocalDateTime.of(2003, 4, 17, 0, 0))

    //~11 months, 22nd of March 2002 through 10th of February 2003
    val interval2 = SimpleInterval(
      LocalDateTime.of(2002, 3, 22, 11, 30, 30, 0), LocalDateTime.of(2003, 2, 10, 22, 10, 20, 0))

    //Friday
    val frInterval1 = RepeatingField(ChronoField.DAY_OF_WEEK, 5)
    //March
    val frInterval2 = RepeatingField(ChronoField.MONTH_OF_YEAR, 3)

    //Interval: Tuesday (1st of February), FieldRI: Friday
    //Expected result: Friday (4th of February)
    var thisRI = ThisRIs(interval, frInterval1).iterator
    var next = thisRI.next
    assert(next.start === LocalDateTime.of(2005, 2, 4, 0, 0))
    assert(next.end === LocalDateTime.of(2005, 2, 5, 0, 0))
    //Expected: Only one element
    assert(thisRI.isEmpty)

    //Interval: Saturday (8th of March) through Friday (14th of March) 2003
    //FieldRI: Friday
    //Expected results: Friday (March 7), Friday (March 14)
    val interval3 = SimpleInterval(LocalDateTime.of(2003, 3, 8, 0, 0), LocalDateTime.of(2003, 3, 14, 0, 0))
    thisRI = ThisRIs(interval3, frInterval1).iterator
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2003, 3, 7, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 3, 8, 0, 0))
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2003, 3, 14, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 3, 15, 0, 0))
    //Expected: Only two elements
    assert(thisRI.isEmpty)

    //Interval: Thursday the 10th through Thursday the 17th of April 2003, FieldRI: Friday,
    //Expected Result: Friday (April 11), Friday (April 18)
    thisRI = ThisRIs(interval1, frInterval1).iterator
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2003, 4, 11, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 4, 12, 0, 0))
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2003, 4, 18, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 4, 19, 0, 0))
    //Expected: Only two elements
    assert(thisRI.isEmpty)

    //Interval: 22nd of March 2002 through 10th of February 2003
    //FieldRI: March
    //Expected Result: March 2002, March 2003
    thisRI = ThisRIs(interval2, frInterval2).iterator
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2002, 3, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2002, 4, 1, 0, 0))
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2003, 3, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 4, 1, 0, 0))
    //Expected: Only two elements
    assert(thisRI.isEmpty)

    //Interval: Thursday the 10th through Thursday the 17th of April 2003, FieldRI: March
    //Expected Result: March 2003
    thisRI = ThisRIs(interval1, frInterval2).iterator
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2003, 3, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 4, 1, 0, 0))
    //Expected: Only one element
    assert(thisRI.isEmpty)

    //Interval: 22nd of March 2002 through 10th of February 2003
    //FieldRI: Fridays
    //Expected Result: All Fridays (48 total)
    thisRI = ThisRIs(interval2, frInterval1).iterator
    assert(thisRI.size == 48)


    //Interval: Tuesday (1st of February 2005), UnitRI: Week
    //Expected result: One week, Monday, January 31st through Sunday, February 6th 2005
    var urInterval = RepeatingUnit(ChronoUnit.WEEKS)
    thisRI = ThisRIs(interval, urInterval).iterator
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2005, 1, 31, 0, 0))
    assert(next.end === LocalDateTime.of(2005, 2, 7, 0, 0))
    assert(thisRI.isEmpty)

    //Interval: Thursday the 10th through Thursday the 17th of April 2003
    //UnitRI: Month
    //Expected Result: April 2003
    urInterval = RepeatingUnit(ChronoUnit.MONTHS)
    thisRI = ThisRIs(interval1, urInterval).iterator
    next = thisRI.next
    assert(next.start === LocalDateTime.of(2003, 4, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 5, 1, 0, 0))
    assert(thisRI.isEmpty)

    //Interval: 22nd of March 2002 through 10th of February 2003
    //UnitRI: Year
    //Expected Result: 2002, 2003
    urInterval = RepeatingUnit(ChronoUnit.YEARS)
    thisRI = ThisRIs(interval2, urInterval).iterator
    next = thisRI.next
    assert(next.start === Year(2002).start)
    assert(next.end === Year(2002).end)
    next = thisRI.next
    assert(next.start === Year(2003).start)
    assert(next.end === Year(2003).end)
    assert(thisRI.isEmpty)


    //Interval: Thursday the 10th through Thursday the 17th of April 2003
    //UnitRI: Day
    //Expected Result: All days in the interval (8 total)
    urInterval = RepeatingUnit(ChronoUnit.DAYS)
    thisRI = ThisRIs(interval1, urInterval).iterator
    assert(thisRI.size == 7)
  }

  test("UnionRI") {
    var interval = SimpleInterval(LocalDateTime.of(2003, 1, 1, 0, 0), LocalDateTime.of(2003, 1, 30, 0, 0))
    val fieldRI = RepeatingField(ChronoField.MONTH_OF_YEAR, 2, Modifier.Exact)
    val fieldRI2 = RepeatingField(ChronoField.DAY_OF_MONTH, 20, Modifier.Exact)
    var unionRI = UnionRI(Set(fieldRI, fieldRI2))

    assert(unionRI.base === ChronoUnit.DAYS)
    assert(unionRI.range === ChronoUnit.YEARS)

    ///////////////////////////////////////////////
    //Interval: January 1, 2003 to January 30, 2003
    //UnionRI: 20th of the month and Februaries
    var unionIterator = unionRI.preceding(interval.start)
    var next = unionIterator.next
    //Expected: December 20, 2002
    assert(next.start === LocalDateTime.of(2002, 12, 20, 0, 0))
    assert(next.end === LocalDateTime.of(2002, 12, 21, 0, 0))

    next = unionIterator.next
    //Expected: November 20, 2002
    assert(next.start === LocalDateTime.of(2002, 11, 20, 0, 0))
    assert(next.end === LocalDateTime.of(2002, 11, 21, 0, 0))

    unionIterator = unionRI.following(interval.end)
    next = unionIterator.next
    //Expected: February 2003
    assert(next.start === LocalDateTime.of(2003, 2, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 3, 1, 0, 0))

    next = unionIterator.next
    //Expected: February 20, 2003
    assert(next.start === LocalDateTime.of(2003, 2, 20, 0, 0))
    assert(next.end === LocalDateTime.of(2003, 2, 21, 0, 0))

    interval = SimpleInterval(LocalDateTime.of(2011, 7, 1, 0, 0), LocalDateTime.of(2011, 7, 30, 0, 0))
    val unitRI = RepeatingUnit(ChronoUnit.DAYS)
    val unitRI1 = RepeatingUnit(ChronoUnit.WEEKS)
    val unitRI2 = RepeatingUnit(ChronoUnit.MONTHS)
    unionRI = UnionRI(Set(unitRI, unitRI1, unitRI2))

    assert(unionRI.base === ChronoUnit.DAYS)
    assert(unionRI.range == ChronoUnit.MONTHS)

    ///////////////////////////////////////////////
    //Interval: July 1, 2003 to July 30, 2011
    //UnionRI: Days, Weeks, and Months
    unionIterator = unionRI.preceding(interval.start)
    next = unionIterator.next
    //Expected: June 2011
    assert(next.start === LocalDateTime.of(2011, 6, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 7, 1, 0, 0))

    next = unionIterator.next
    //Expected: June 30, 2011
    assert(next.start === LocalDateTime.of(2011, 6, 30, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 7, 1, 0, 0))

    unionIterator = unionRI.following(interval.end)
    next = unionIterator.next
    //Expected: July 30, 2011
    assert(next.start === LocalDateTime.of(2011, 7, 30, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 7, 31, 0, 0))

    next = unionIterator.next
    //Expected: July 31, 2011
    assert(next.start === LocalDateTime.of(2011, 7, 31, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 8, 1, 0, 0))

    next = unionIterator.next
    //Expected: August 1, 2011
    assert(next.start === LocalDateTime.of(2011, 8, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 8, 2, 0, 0))

    next = unionIterator.next
    //Expected: August 1 through August 7 2011 (August 1, 2011 is a Monday)
    assert(next.start === LocalDateTime.of(2011, 8, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 8, 8, 0, 0))

    ///////////////////////////////////////////////
    //FieldRI with UnitRI
    //
    //Interval: July 1 to July 30, 2011
    //UnitRI: Weeks
    //FieldRI: 20th of the month
    val fieldRI3 = RepeatingField(ChronoField.DAY_OF_MONTH, 20, Modifier.Exact)
    unionRI = UnionRI(Set(unitRI1, fieldRI3))

    //Preceding
    unionIterator = unionRI.preceding(interval.start)
    next = unionIterator.next
    //Expected: June 20 through June 26, 2003 (June 20, 2011 is a Monday)
    assert(next.start === LocalDateTime.of(2011, 6, 20, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 6, 27, 0, 0))

    next = unionIterator.next
    //Expected: June 20, 2003
    assert(next.start === LocalDateTime.of(2011, 6, 20, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 6, 21, 0, 0))

    //Following
    unionIterator = unionRI.following(interval.end)
    next = unionIterator.next
    //Expected: August 1 through August 7 2011 (August 1, 2011 is a Monday)
    assert(next.start === LocalDateTime.of(2011, 8, 1, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 8, 8, 0, 0))

    next = unionIterator.next
    //Expected: August 8 through August 14 2011
    assert(next.start === LocalDateTime.of(2011, 8, 8, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 8, 15, 0, 0))

    next = unionIterator.next
    //Expected: August 15 through August 21, 2011
    assert(next.start === LocalDateTime.of(2011, 8, 15, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 8, 22, 0, 0))

    next = unionIterator.next
    //Expected: August 20, 2011
    assert(next.start === LocalDateTime.of(2011, 8, 20, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 8, 21, 0, 0))

    ///////////////////////////////////////////////
    //ThisRI with UnionRI
    //
    //Interval: July 1 to July 30, 2011
    //UnitRI: Weeks
    //FieldRI: 20th of the month
    val thisRI = ThisRIs(interval, unionRI).iterator
    next = thisRI.next
    //Expected: July 4 through July 10, 2011 (July 4, 2011 is a Monday)
    assert(next.start === LocalDateTime.of(2011, 7, 4, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 7, 11, 0, 0))

    next = thisRI.next
    //Expected: July 11 through July 17, 2011
    assert(next.start === LocalDateTime.of(2011, 7, 11, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 7, 18, 0, 0))

    next = thisRI.next
    //Expected: July 18 through July 24, 2011
    assert(next.start === LocalDateTime.of(2011, 7, 18, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 7, 25, 0, 0))

    next = thisRI.next
    //Expected: July 20, 2011
    assert(next.start === LocalDateTime.of(2011, 7, 20, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 7, 21, 0, 0))

    next = thisRI.next
    //Expected: July 25 through July 31, 2011
    assert(next.start === LocalDateTime.of(2011, 7, 25, 0, 0))
    assert(next.end === LocalDateTime.of(2011, 8, 1, 0, 0))

    //Expected: No further intervals
    assert(thisRI.isEmpty)

  }

  test("IntersectionRI") {
    //Interval: The year of 2016
    val interval = Year(2016)
    //January, Friday the 13th
    var intersectRI = IntersectionRI(
      Set(
        RepeatingField(ChronoField.DAY_OF_WEEK, 5, Modifier.Exact), //Friday
        RepeatingField(ChronoField.DAY_OF_MONTH, 13, Modifier.Exact), //the 13th
        RepeatingField(ChronoField.MONTH_OF_YEAR, 1, Modifier.Exact)) //January
    )

    assert(intersectRI.base === ChronoUnit.DAYS)
    assert(intersectRI.range === ChronoUnit.YEARS)

    //Interval: The year of 2016
    var following = intersectRI.following(interval.end)
    //Expected: Friday, January 13, 2017
    var next = following.next
    assert(next.start === LocalDateTime.of(2017, 1, 13, 0, 0))
    assert(next.start.getDayOfWeek == DayOfWeek.FRIDAY)
    assert(next.end === LocalDateTime.of(2017, 1, 14, 0, 0))

    //Expected: Friday, January 13, 2023
    next = following.next
    assert(next.start === LocalDateTime.of(2023, 1, 13, 0, 0))
    assert(next.start.getDayOfWeek == DayOfWeek.FRIDAY)
    assert(next.end === LocalDateTime.of(2023, 1, 14, 0, 0))

    //Interval: The year of 2016
    var preceding = intersectRI.preceding(interval.start)
    //Expected: Friday, January 13, 2012
    next = preceding.next
    assert(next.start === LocalDateTime.of(2012, 1, 13, 0, 0))
    assert(next.start.getDayOfWeek == DayOfWeek.FRIDAY)
    assert(next.end === LocalDateTime.of(2012, 1, 14, 0, 0))

    //Expected: Friday, January 13, 2006
    next = preceding.next
    assert(next.start === LocalDateTime.of(2006, 1, 13, 0, 0))
    assert(next.start.getDayOfWeek == DayOfWeek.FRIDAY)
    assert(next.end === LocalDateTime.of(2006, 1, 14, 0, 0))

    //RI: The hours of Friday the 13th
    intersectRI = IntersectionRI(
      Set(
        RepeatingField(ChronoField.DAY_OF_WEEK, 5, Modifier.Exact), //Friday
        RepeatingField(ChronoField.DAY_OF_MONTH, 13, Modifier.Exact), //the 13th
        RepeatingUnit(ChronoUnit.HOURS) //Hours
      ))
    //Interval: The year of 2016
    following = intersectRI.following(interval.end)
    //Expected: Friday, January 13, 2017 @ 0000
    next = following.next
    assert(next.start === LocalDateTime.of(2017, 1, 13, 0, 0))
    assert(next.end === LocalDateTime.of(2017, 1, 13, 1, 0))

    //Expected: (10 hours after previous time) Friday, January 13, 2017 @ 1000
    next = following.drop(9).next
    assert(next.start === LocalDateTime.of(2017, 1, 13, 10, 0))
    assert(next.end === LocalDateTime.of(2017, 1, 13, 11, 0))

    preceding = intersectRI.preceding(interval.start)
    //Expected: Friday, November 13, 2015 @ 2300 (Final hour of the day)
    next = preceding.next
    assert(next.start === LocalDateTime.of(2015, 11, 13, 23, 0))
    assert(next.end === LocalDateTime.of(2015, 11, 14, 0, 0))

    //Expected: (10 hours previous) Friday, November 13, 2015 @ 1300
    next = preceding.drop(9).next
    assert(next.start === LocalDateTime.of(2015, 11, 13, 13, 0))
    assert(next.end === LocalDateTime.of(2015, 11, 13, 14, 0))

    val march = RepeatingField(ChronoField.MONTH_OF_YEAR, 3)
    val day31 = RepeatingField(ChronoField.DAY_OF_MONTH, 31)
    val mar31 = IntersectionRI(Set(march, day31))
    val startOf1980 = LocalDateTime.of(1980, 1, 1, 0, 0)
    assert(mar31.following(startOf1980).next === SimpleInterval.of(1980, 3, 31))
  }

  test("IntersectionI") {
    assert(IntersectionI(Seq(SimpleInterval.of(1998), SimpleInterval.of(1998, 2))) === SimpleInterval.of(1998, 2))
    assert(IntersectionI(Seq(
      SimpleInterval(LocalDateTime.of(2017, 4, 25, 0, 0), LocalDateTime.of(2017, 6, 30, 0, 0)),
      SimpleInterval(LocalDateTime.of(2017, 2, 2, 0, 0), LocalDateTime.of(2017, 5, 7, 0, 0)),
      SimpleInterval(LocalDateTime.of(2017, 3, 15, 0, 0), LocalDateTime.of(2017, 4, 28, 0, 0))))
      === SimpleInterval(LocalDateTime.of(2017, 4, 25, 0, 0), LocalDateTime.of(2017, 4, 28, 0, 0)))

    // two intervals that don't intersect at all
    intercept[UnsupportedOperationException] {
      val Interval(_, _) = IntersectionI(Seq(SimpleInterval.of(1998), SimpleInterval.of(2000)))
    }

    // three intervals that intersect in pairs, but not all together
    intercept[UnsupportedOperationException] {
      val Interval(_, _) = IntersectionI(Seq(
        SimpleInterval(LocalDateTime.of(2017, 4, 25, 0, 0), LocalDateTime.of(2017, 6, 30, 0, 0)),
        SimpleInterval(LocalDateTime.of(2017, 2, 2, 0, 0), LocalDateTime.of(2017, 5, 7, 0, 0)),
        SimpleInterval(LocalDateTime.of(2017, 3, 15, 0, 0), LocalDateTime.of(2017, 4, 25, 0, 0))))
    }
  }

  test("isDefined") {
    val threeDays = SimplePeriod(ChronoUnit.DAYS, 3)
    val fridays = RepeatingField(ChronoField.DAY_OF_WEEK, 5)
    assert(threeDays.isDefined === true)
    assert(UnknownPeriod.isDefined === false)
    assert(SumP(Set(threeDays, threeDays)).isDefined === true)
    assert(SumP(Set(threeDays, UnknownPeriod)).isDefined === false)
    assert(LastRI(SimpleInterval.of(1998, 2, 16), fridays).isDefined === true)
    assert(AfterRI(Year(1965), fridays).isDefined === true)
  }

  test("PRI19980216.2000.0170 (349,358) last week") {
    assert(
      LastRI(SimpleInterval.of(1998, 2, 16), RepeatingUnit(ChronoUnit.WEEKS))
        === SimpleInterval(LocalDateTime.of(1998, 2, 9, 0, 0), LocalDateTime.of(1998, 2, 16, 0, 0)))
  }

  test("APW19980322.0749 (988,994) Monday") {
    assert(
      NextRI(SimpleInterval.of(1998, 3, 22, 14, 57), RepeatingField(ChronoField.DAY_OF_WEEK, 1))
        === SimpleInterval.of(1998, 3, 23))
  }

  test("APW19990206.0090 (767,781) Thursday night") {
    val dct = SimpleInterval.of(1999, 2, 6, 6, 22, 26)
    val thursday = RepeatingField(ChronoField.DAY_OF_WEEK, 4)
    val night = RepeatingField(NIGHT_OF_DAY, 1)
    assert(
      LastRI(dct, IntersectionRI(Set(thursday, night)))
        === SimpleInterval(LocalDateTime.of(1999, 2, 4, 21, 0), LocalDateTime.of(1999, 2, 5, 4, 0)))
  }

  test("wsj_0124 (450,457) Nov. 13") {
    val nov13 = IntersectionRI(
      Set(RepeatingField(ChronoField.MONTH_OF_YEAR, 11), RepeatingField(ChronoField.DAY_OF_MONTH, 13)))
    assert(NextRI(SimpleInterval.of(1989, 11, 2), nov13) === SimpleInterval.of(1989, 11, 13))
    assert(LastRI(SimpleInterval.of(1989, 11, 14), nov13) === SimpleInterval.of(1989, 11, 13))
    assert(NextRI(SimpleInterval.of(1989, 11, 12), nov13) === SimpleInterval.of(1989, 11, 13))
  }

  test("NYT19980206.0460 (2979,3004) first nine months of 1997") {
    assert(
      NthRIs(Year(1997), 1, RepeatingUnit(ChronoUnit.MONTHS), 9)
        === SimpleIntervals((1 to 9).map(m => SimpleInterval.of(1997, m))))
  }

  test("wsj_0346 (889,894) year ended March 31") {
    val march = RepeatingField(ChronoField.MONTH_OF_YEAR, 3)
    val day31 = RepeatingField(ChronoField.DAY_OF_MONTH, 31)
    val march31 = IntersectionRI(Set(march, day31))
    assert(
      LastP(LastRI(SimpleInterval.of(1989, 11, 1), march31), SimplePeriod(ChronoUnit.YEARS, 1))
        === SimpleInterval(LocalDateTime.of(1988, 3, 31, 0, 0), LocalDateTime.of(1989, 3, 31, 0, 0)))
  }
}

