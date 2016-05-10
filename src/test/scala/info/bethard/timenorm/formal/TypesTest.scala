package info.bethard.timenorm.formal

import java.time.temporal.{ChronoField, ChronoUnit, TemporalUnit, UnsupportedTemporalTypeException}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.{DateTimeException, LocalDateTime}
import java.util.Collections.singletonList

@RunWith(classOf[JUnitRunner])
class TypesTest extends FunSuite {
  
  test( "Year Type" ) {
    val year = Year( 1985 )
    
    assert( year.start === LocalDateTime.of( 1985, 1, 1, 0, 0, 0, 0 ))
    assert( year.end === LocalDateTime.of( 1986, 1, 1, 0, 0, 0, 0 ) )
  }
  
  test( "Decade Type" ) {
    val decade = Decade( 198 )
    
    assert( decade.start === LocalDateTime.of( 1980, 1, 1, 0, 0, 0, 0 ))
    assert( decade.end === LocalDateTime.of( 1990, 1, 1, 0, 0, 0, 0 )) 
  }
  
  test( "Century Type" ) {
    val century = Century( 17 )
    
    assert( century.start === LocalDateTime.of( 1700, 1, 1, 0, 0, 0, 0 ))
    assert( century.end === LocalDateTime.of( 1800, 1, 1, 0, 0, 0, 0 )) 
  }
  
  test( "TwoDigitYear Type" ) {
    val fromYear = TwoDigitYear( Year( 1903 ), 37 )
    assert( fromYear.start === LocalDateTime.of( 1937, 1, 1, 0, 0, 0, 0 ))
    assert( fromYear.end === LocalDateTime.of( 1938, 1, 1, 0, 0, 0, 0 )) 
    
    val fromDecade = TwoDigitYear( Decade( 132 ), 85 )
    assert( fromDecade.start === LocalDateTime.of( 1385, 1, 1, 0, 0, 0, 0 ))
    assert( fromDecade.end === LocalDateTime.of( 1386, 1, 1, 0, 0, 0, 0 )) 
    
    val fromCentury = TwoDigitYear( Century( 23 ), 22 )
    assert( fromCentury.start === LocalDateTime.of( 2322, 1, 1, 0, 0, 0, 0 ))
    assert( fromCentury.end === LocalDateTime.of( 2323, 1, 1, 0, 0, 0, 0 )) 
    
  }

  test( "SimplePeriod" ) {
    val ldt = LocalDateTime.of( 2000, 1, 1, 0, 0, 0, 0 )
    val number = IntNumber( 5 )
    val unit = ChronoUnit.YEARS
    val mod = Modifier.Exact

    val simple = SimplePeriod( unit, number, mod )
    assert( simple.addTo( ldt ) === LocalDateTime.of( 2005, 1, 1, 0, 0, 0, 0 ))
    assert( simple.subtractFrom( ldt ) === LocalDateTime.of( 1995, 1, 1, 0, 0, 0, 0))
    assert( simple.get( unit ) === 5 )
    assert( simple.getUnits() === singletonList(unit) )

    //Expected failures to follow
    intercept [UnsupportedTemporalTypeException] {
      assert( simple.get( ChronoUnit.MONTHS ) === 60 )
    }

    val vagueNumber = VagueNumber("A few")

    intercept [scala.NotImplementedError] {
      val simpleVague = SimplePeriod(unit, vagueNumber, mod)
    }
  }

  test( "PeriodSum" ) {
    val period1 = SimplePeriod(ChronoUnit.YEARS, IntNumber(1), Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, IntNumber(2), Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, IntNumber(3), Modifier.Approx)
    val period4 = SimplePeriod(ChronoUnit.DAYS, IntNumber(2), Modifier.Mid )
    val periodSum = PeriodSum( Set(period1, period2, period3), Modifier.Exact)
    val ldt = LocalDateTime.of(2000, 6, 10, 0, 0, 0, 0)

    val list = new java.util.ArrayList[TemporalUnit]
    list.add(ChronoUnit.YEARS)
    list.add(ChronoUnit.MONTHS)

    assert( periodSum.addTo(ldt) === LocalDateTime.of(2003, 9, 10, 0, 0, 0, 0))
    assert( periodSum.subtractFrom(ldt) === LocalDateTime.of(1997, 3, 10, 0, 0, 0, 0))
    assert( periodSum.get(ChronoUnit.YEARS) == 3)
    assert( periodSum.getUnits() === list )

    //Tests for periodSums that contain periodSums
    val periodSum2 = PeriodSum( Set( period4, periodSum), Modifier.Fiscal )
    list.add(ChronoUnit.DAYS)

    assert( periodSum2.addTo( ldt ) === LocalDateTime.of( 2003, 9, 12, 0, 0, 0, 0))
    assert( periodSum2.subtractFrom(ldt) === LocalDateTime.of( 1997, 3, 8, 0, 0, 0, 0))
    assert( periodSum2.get(ChronoUnit.DAYS) == 2)
    assert( periodSum2.getUnits() === list)

    intercept [UnsupportedTemporalTypeException] {
      periodSum2.get(ChronoUnit.HOURS)
    }
  }

  test( "LastPeriod" ) {
    val period1 = SimplePeriod(ChronoUnit.YEARS, IntNumber(1), Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, IntNumber(2), Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, IntNumber(3), Modifier.Approx)
    val periodSum = PeriodSum( Set(period1, period2, period3), Modifier.Exact)

    val year = Year( 2000 )
    val lastPeriod = LastPeriod( year, period1 )

    assert( lastPeriod.end === year.start )
    assert( lastPeriod.start === LocalDateTime.of( 1999, 1, 1, 0, 0, 0, 0 ))

    val lastPeriod1 = LastPeriod( year, periodSum )

    assert( lastPeriod1.end === year.start )
    assert( lastPeriod1.start === LocalDateTime.of( 1996, 10, 1, 0, 0, 0, 0 ))
  }

  test( "NextPeriod" ) {
    val period1 = SimplePeriod(ChronoUnit.YEARS, IntNumber(1), Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, IntNumber(2), Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, IntNumber(3), Modifier.Approx)
    val periodSum = PeriodSum( Set(period1, period2, period3), Modifier.Exact)

    val year = Year( 2000 )
    val nextPeriod = NextPeriod( year, period1 )

    assert( nextPeriod.start === year.end )
    assert( nextPeriod.end === LocalDateTime.of( 2001, 1, 1, 0, 0, 0, 0 ))

    val nextPeriod1 = NextPeriod( year, periodSum )

    assert( nextPeriod1.start === year.end )
    assert( nextPeriod1.end === LocalDateTime.of( 2003, 4, 1, 0, 0, 0, 0 ))
  }

  test( "BeforePeriod" ) {
    val period1 = SimplePeriod(ChronoUnit.YEARS, IntNumber(1), Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, IntNumber(2), Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, IntNumber(3), Modifier.Approx)
    val periodSum = PeriodSum( Set(period1, period2, period3), Modifier.Exact)

    val year = Year( 2000 )
    val beforePeriod = BeforePeriod( year, period1 )

    assert( beforePeriod.start === LocalDateTime.of( 1999, 1, 1, 0, 0, 0, 0 ))
    assert( beforePeriod.end === LocalDateTime.of( 2000, 1, 1, 0, 0, 0, 0 ))

    val beforePeriod1 = BeforePeriod( year, periodSum )

    assert( beforePeriod1.start === LocalDateTime.of( 1996, 10, 1, 0, 0, 0, 0 ))
    assert( beforePeriod1.end === LocalDateTime.of( 1997, 10, 1, 0, 0, 0, 0 ))
  }

  test( "AfterPeriod" ) {
    val period1 = SimplePeriod(ChronoUnit.YEARS, IntNumber(1), Modifier.Exact)
    val period2 = SimplePeriod(ChronoUnit.YEARS, IntNumber(2), Modifier.Fiscal)
    val period3 = SimplePeriod(ChronoUnit.MONTHS, IntNumber(3), Modifier.Approx)
    val periodSum = PeriodSum( Set(period1, period2, period3), Modifier.Exact)

    val year = Year( 2000 )
    val afterPeriod = AfterPeriod( year, period1 )

    assert( afterPeriod.start === LocalDateTime.of( 2001, 1, 1, 0, 0, 0, 0 ))
    assert( afterPeriod.end === LocalDateTime.of( 2002, 1, 1, 0, 0, 0, 0 ))

    val afterPeriod1 = AfterPeriod( year, periodSum )

    assert( afterPeriod1.start === LocalDateTime.of( 2003, 4, 1, 0, 0, 0, 0 ))
    assert( afterPeriod1.end === LocalDateTime.of( 2004, 4, 1, 0, 0, 0, 0 ))
  }

  test( "ThisPeriod" ) {
    val period1 = SimplePeriod(ChronoUnit.YEARS, IntNumber(1), Modifier.Exact)

    val year = Year( 2002 )
    val thisPeriod = ThisPeriod( year, period1 )

    assert( thisPeriod.start === LocalDateTime.of(2002, 1, 1, 0, 0, 0 ,0))
    assert( thisPeriod.end === LocalDateTime.of( 2003, 1, 1, 0, 0, 0, 0))

    val interval = new Interval {
      val start = LocalDateTime.of(2001, 1, 1, 0, 0, 0, 0)
      val end = LocalDateTime.of(2001, 1, 1, 0, 0, 0, 0)
    }
    val period = SimplePeriod(ChronoUnit.DAYS, IntNumber(5), Modifier.Exact)
    val thisPeriod2 = ThisPeriod(interval, period)

    assert( thisPeriod2.start === LocalDateTime.of(2000, 12, 29, 12, 0, 0, 0))
    assert( thisPeriod2.end === LocalDateTime.of( 2001, 1, 3, 12, 0, 0, 0))
  }

  test( "Between" ) {
    val interval1 = Year(1999)
    val interval2 = Year(2002)
    val between = Between(interval1,interval2)

    assert( between.start === LocalDateTime.of(2000,1,1,0,0,0,0))
    assert( between.end === LocalDateTime.of(2002,1,1,0,0,0,0))
  }

  test( "Nth" ) {
    val period1 = SimplePeriod(ChronoUnit.YEARS, IntNumber(1), Modifier.Exact)

    val year = Year( 2001 )
    val nth = NthInterval( year, IntNumber(2), period1 )

    assert( nth.start === LocalDateTime.of(2002, 1, 1, 0, 0, 0, 0))
    assert( nth.end === LocalDateTime.of(2003, 1, 1, 0, 0, 0, 0))

    val period2 = SimplePeriod(ChronoUnit.MINUTES, IntNumber(20), Modifier.Exact)
    val periodSum = PeriodSum( Set(period1,period2), Modifier.Exact)
    val nth2 = NthInterval( year, IntNumber(4), periodSum)

    assert( nth2.start === LocalDateTime.of(2004, 1, 1, 1, 0, 0, 0))
    assert( nth2.end === LocalDateTime.of(2005, 1, 1, 1, 20, 0, 0))
  }

  test( "UnitRepeatingInterval" ) {
    val rInterval = UnitRepeatingInterval( ChronoUnit.MONTHS, Modifier.Exact )
    val ldt1 = LocalDateTime.of(2002,3,22,11,30,30,0)
    val ldt2 = LocalDateTime.of(2003,5,10,22,10,20,0)
    val interval = SimpleInterval(ldt1,ldt2)

    val pre = rInterval.preceding(interval.start)
    var next = pre.next()
    assert( next.start === LocalDateTime.of( 2002, 2, 1, 0, 0, 0, 0))
    assert( next.end === LocalDateTime.of( 2002, 3, 1, 0, 0, 0, 0))

    next = pre.next()
    assert( next.start === LocalDateTime.of( 2002, 1, 1, 0, 0, 0, 0))
    assert( next.end === LocalDateTime.of( 2002, 2, 1, 0, 0, 0, 0))

    val follow = rInterval.following(interval.end)
    next = follow.next()
    assert (next.start === LocalDateTime.of( 2003, 6, 1, 0, 0, 0, 0))
    assert( next.end === LocalDateTime.of( 2003, 7, 1, 0, 0, 0, 0))

    next = follow.next()
    assert (next.start === LocalDateTime.of( 2003, 7, 1, 0, 0, 0, 0))
    assert( next.end === LocalDateTime.of( 2003, 8, 1, 0, 0, 0, 0))

    //Truncate method tests
    val mod = Modifier.End
    val centuryRI = UnitRepeatingInterval(ChronoUnit.CENTURIES, mod)
    val decadeRI = UnitRepeatingInterval(ChronoUnit.DECADES, mod)
    val weeksRI = UnitRepeatingInterval(ChronoUnit.WEEKS, mod)

    var centuries = centuryRI.preceding(interval.start)
    next = centuries.next
    assert (next.start === LocalDateTime.of(1900,1,1,0,0))
    assert (next.end === LocalDateTime.of(2000,1,1,0,0))

    centuries = centuryRI.following(interval.end)
    next = centuries.next
    assert (next.start === LocalDateTime.of(2100,1,1,0,0))
    assert (next.end === LocalDateTime.of(2200,1,1,0,0))

    var decades = decadeRI.preceding(interval.start)
    next = decades.next
    assert (next.start === LocalDateTime.of(1990,1,1,0,0))
    assert (next.end === LocalDateTime.of(2000,1,1,0,0))

    decades = decadeRI.following(interval.end)
    next = decades.next
    assert (next.start === LocalDateTime.of(2010,1,1,0,0))
    assert (next.end === LocalDateTime.of(2020,1,1,0,0))

    var weeks = weeksRI.preceding(interval.start)
    next = weeks.next
    assert (next.start === LocalDateTime.of(2002,3,10,0,0))
    assert (next.end === LocalDateTime.of(2002,3,17,0,0))

    weeks = weeksRI.following(interval.end)
    next = weeks.next
    assert (next.start === LocalDateTime.of(2003,5,11,0,0))
    assert (next.end === LocalDateTime.of(2003,5,18,0,0))
  }

  test( "FieldRepeatingInterval" ) {
    val rInterval = FieldRepeatingInterval( ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact )
    val rInterval2 = FieldRepeatingInterval( ChronoField.DAY_OF_MONTH, 29, Modifier.Exact )
    val interval = SimpleInterval(
      LocalDateTime.of(2002,3,22,11,30,30,0), LocalDateTime.of(2003,5,10,22,10,20,0))

    ////Tests for MONTH_OF_YEAR////

    //  Preceding
    val pre = rInterval.preceding(interval.start)
    var next = pre.next()
    assert( next.start === LocalDateTime.of(2001,5,1,0,0,0,0))
    assert( next.end === LocalDateTime.of(2001,6,1,0,0,0,0))

    next = pre.next()
    assert( next.start === LocalDateTime.of(2000,5,1,0,0,0,0))
    assert( next.end === LocalDateTime.of(2000,6,1,0,0,0,0))

    //  Following
    val post = rInterval.following(interval.end)
    next = post.next()
    assert( next.start === LocalDateTime.of(2004,5,1,0,0,0,0))
    assert( next.end === LocalDateTime.of(2004,6,1,0,0,0,0))

    next = post.next()
    assert( next.start === LocalDateTime.of(2005,5,1,0,0,0,0))
    assert( next.end === LocalDateTime.of(2005,6,1,0,0,0,0))

    ////Tests for DAY_OF_MONTH////

    //  Preceding
    val pre2 = rInterval2.preceding(interval.start)
    next = pre2.next()
    assert( next.start === LocalDateTime.of(2002,1,29,0,0,0,0))
    assert( next.end === LocalDateTime.of(2002,1,30,0,0,0,0))

    next = pre2.next()
    assert( next.start === LocalDateTime.of(2001,12,29,0,0,0,0))
    assert( next.end === LocalDateTime.of(2001,12,30,0,0,0,0))

    //  Following
    val post2 = rInterval2.following(interval.end)
    next = post2.next()
    assert( next.start === LocalDateTime.of(2003,5,29,0,0,0,0))
    assert( next.end === LocalDateTime.of(2003,5,30,0,0,0,0))

    next = post2.next()
    assert( next.start === LocalDateTime.of(2003,6,29,0,0,0,0))
    assert( next.end === LocalDateTime.of(2003,6,30,0,0,0,0))

    //No Exception at FieldRepeatingInterval instantiation
    val rInterval3 = FieldRepeatingInterval( ChronoField.DAY_OF_MONTH, 300, Modifier.Approx)
    intercept [DateTimeException] {
      //Exception thrown here
      val testException = rInterval3.preceding(interval.start)
    }
  }

  test( "LastRepeatingInterval" ) {
    val interval = SimpleInterval(
      LocalDateTime.of(2002,3,22,11,30,30,0), LocalDateTime.of(2003,5,10,22,10,20,0))
    val frInterval = FieldRepeatingInterval( ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact )
    val urInterval = UnitRepeatingInterval(ChronoUnit.DAYS, Modifier.Exact)

    val lastFieldRI = LastRepeatingInterval(interval, frInterval)
    assert( lastFieldRI.start === LocalDateTime.of(2001,5,1,0,0,0,0))
    assert( lastFieldRI.end === LocalDateTime.of(2001,6,1,0,0,0,0))

    val lastUnitRI = LastRepeatingInterval(interval, urInterval)
    assert( lastUnitRI.start === LocalDateTime.of(2002,3,21,0,0,0,0))
    assert( lastUnitRI.end === LocalDateTime.of(2002,3,22,0,0,0,0))
  }

  test( "NextRepeatingInterval" ) {
    val interval = SimpleInterval(
      LocalDateTime.of(2002,3,22,11,30,30,0), LocalDateTime.of(2003,5,10,22,10,20,0))
    val frInterval = FieldRepeatingInterval( ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact )
    val urInterval = UnitRepeatingInterval(ChronoUnit.DAYS, Modifier.Exact)

    val nextFieldRI = NextRepeatingInterval(interval, frInterval)
    assert( nextFieldRI.start === LocalDateTime.of(2004,5,1,0,0,0,0))
    assert( nextFieldRI.end === LocalDateTime.of(2004,6,1,0,0,0,0))

    val nextUnitRI = NextRepeatingInterval(interval, urInterval)
    assert( nextUnitRI.start === LocalDateTime.of(2003,5,11,0,0,0,0))
    assert( nextUnitRI.end === LocalDateTime.of(2003,5,12,0,0,0,0))
  }

  test( "AfterRepeatingInterval" ) {
    val interval = SimpleInterval(
      LocalDateTime.of(2002,3,22,11,30,30,0), LocalDateTime.of(2003,5,10,22,10,20,0))
    val frInterval = FieldRepeatingInterval( ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact )
    val urInterval = UnitRepeatingInterval(ChronoUnit.DAYS, Modifier.Exact)

    val afterFieldRI = AfterRepeatingInterval(interval, frInterval)
    assert( afterFieldRI.start === LocalDateTime.of(2004,5,1,0,0,0,0))
    assert( afterFieldRI.end === LocalDateTime.of(2004,6,1,0,0,0,0))

    val afterUnitRI = AfterRepeatingInterval(interval, urInterval)
    assert( afterUnitRI.start === LocalDateTime.of(2003,5,11,0,0,0,0))
    assert( afterUnitRI.end === LocalDateTime.of(2003,5,12,0,0,0,0))
  }

  test ( "BeforeRepeatingInterval" ) {
    val interval = SimpleInterval(
      LocalDateTime.of(2002,3,22,11,30,30,0), LocalDateTime.of(2003,5,10,22,10,20,0))
    val frInterval = FieldRepeatingInterval( ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact )
    val urInterval = UnitRepeatingInterval(ChronoUnit.DAYS, Modifier.Exact)

    val beforeFieldRI = BeforeRepeatingInterval(interval, frInterval)
    assert( beforeFieldRI.start === LocalDateTime.of(2001,5,1,0,0,0,0))
    assert( beforeFieldRI.end === LocalDateTime.of(2001,6,1,0,0,0,0))

    val beforeUnitRI = BeforeRepeatingInterval(interval, urInterval)
    assert( beforeUnitRI.start === LocalDateTime.of(2002,3,21,0,0,0,0))
    assert( beforeUnitRI.end === LocalDateTime.of(2002,3,22,0,0,0,0))
  }

  test ( "NthRepeatingInterval" ) {
    val interval = SimpleInterval(
      LocalDateTime.of(2002,3,22,11,30,30,0), LocalDateTime.of(2003,5,10,22,10,20,0))
    val frInterval = FieldRepeatingInterval( ChronoField.MONTH_OF_YEAR, 5, Modifier.Exact )
    val urInterval = UnitRepeatingInterval(ChronoUnit.DAYS, Modifier.Exact)

    val nthFieldRI = Nth(interval, 1, frInterval)
    assert( nthFieldRI.start === LocalDateTime.of(2002, 5, 1, 0, 0, 0, 0))
    assert( nthFieldRI.end === LocalDateTime.of(2002, 6, 1, 0, 0, 0, 0))

    val nthUnitRI = Nth(interval, 3, urInterval)
    assert( nthUnitRI.start === LocalDateTime.of(2002, 3, 25, 0, 0, 0, 0))
    assert( nthUnitRI.end === LocalDateTime.of(2002, 3, 26, 0, 0, 0, 0))

    intercept [NotImplementedError] {
      val nthFailure = Nth(interval, 5, frInterval)
    }
  }

  test ( "ThisRepeatingInterval" ) {
    //~One day, Tuesday (1st of February 2005)
    val interval = SimpleInterval(
      LocalDateTime.of(2005,2,1,3,22), LocalDateTime.of(2005,2,2,0,0))

    //One week, Thursday the 10th through Thursday the 17th of April 2003
    val interval1 = SimpleInterval(
      LocalDateTime.of(2003, 4, 10, 0, 0), LocalDateTime.of(2003, 4, 17, 0, 0))

    //~11 months, 22nd of March 2002 through 10th of February 2003
    val interval2 = SimpleInterval(
      LocalDateTime.of(2002,3,22,11,30,30,0), LocalDateTime.of(2003,2,10,22,10,20,0))

    //Friday
    val frInterval1 = FieldRepeatingInterval( ChronoField.DAY_OF_WEEK, 5, Modifier.Exact )
    //March
    val frInterval2 = FieldRepeatingInterval( ChronoField.MONTH_OF_YEAR, 3, Modifier.Exact )

    //Interval: Tuesday (1st of February), FieldRI: Friday
    //Expected result: Friday (4th of February)
    var thisRI = ThisRepeatingInterval(interval, frInterval1)
    assert( thisRI.start === LocalDateTime.of(2005,2,4,0,0))
    assert( thisRI.end === LocalDateTime.of(2005,2,5,0,0))

    //Interval: Saturday (8th of March) through Friday (14th of March) 2003
    //FieldRI: Friday
    //Expected result: Friday (14th of March) 2003
    val interval3 = SimpleInterval(LocalDateTime.of(2003, 3, 8, 0, 0), LocalDateTime.of(2003, 3, 14, 0, 0))
    thisRI = ThisRepeatingInterval(interval3, frInterval1)
    assert( thisRI.start === LocalDateTime.of(2003,3,14,0,0))
    assert( thisRI.end === LocalDateTime.of(2003,3,15,0,0))

    //Interval: Thursday the 10th through Thursday the 17th of April 2003, FieldRI: Friday,
    //Expected Result: Friday (11th of April)
    thisRI = ThisRepeatingInterval(interval1, frInterval1)
    assert( thisRI.start === LocalDateTime.of(2003, 4, 11, 0, 0))
    assert( thisRI.end === LocalDateTime.of( 2003, 4, 12, 0, 0))

    //Interval: 22nd of March 2002 through 10th of February 2003
    //FieldRI: March
    //Expected Result: March 2002
    thisRI = ThisRepeatingInterval(interval2, frInterval2)
    assert( thisRI.start === LocalDateTime.of(2002, 3, 1, 0, 0))
    assert( thisRI.end === LocalDateTime.of(2002, 4, 1, 0, 0))

    //Interval: Thursday the 10th through Thursday the 17th of April 2003, FieldRI: March
    //Expected Result: March 2003
    thisRI = ThisRepeatingInterval(interval1, frInterval2)
    assert( thisRI.start === LocalDateTime.of(2003, 3, 1, 0, 0))
    assert( thisRI.end === LocalDateTime.of(2003, 4, 1, 0, 0))

    intercept [DateTimeException] {
      thisRI = ThisRepeatingInterval(interval2, frInterval1)
    }

    //Interval: Tuesday (1st of February 2005), UnitRI: Week
    //Expected result: Sunday, January 30th through Saturday, February 5th 2005
    var urInterval = UnitRepeatingInterval( ChronoUnit.WEEKS, Modifier.Exact)
    thisRI = ThisRepeatingInterval(interval, urInterval)
    assert( thisRI.start === LocalDateTime.of(2005, 1, 30, 0, 0))
    assert( thisRI.end === LocalDateTime.of(2005, 2, 6, 0, 0))

    //Interval: Thursday the 10th through Thursday the 17th of April 2003
    //UnitRI: Month
    //Expected Result: April 2003
    urInterval = UnitRepeatingInterval(ChronoUnit.MONTHS, Modifier.Exact)
    thisRI = ThisRepeatingInterval(interval1, urInterval)
    assert( thisRI.start === LocalDateTime.of(2003, 4, 1, 0, 0))
    assert( thisRI.end === LocalDateTime.of(2003, 5, 1, 0, 0))

    //Interval: 22nd of March 2002 through 10th of February 2003
    //UnitRI: Year
    //Expected Result: Year of 2003
    urInterval = UnitRepeatingInterval(ChronoUnit.YEARS, Modifier.Exact)
    thisRI = ThisRepeatingInterval(interval2, urInterval)
    assert( thisRI.start === Year(2003).start)
    assert( thisRI.end === Year(2003).end)

    //Interval: Thursday the 10th through Thursday the 17th of April 2003
    //UnitRI: Day
    //Expected Result: DateTimeException
    urInterval = UnitRepeatingInterval(ChronoUnit.DAYS, Modifier.Exact)
    intercept [DateTimeException] {
      thisRI = ThisRepeatingInterval(interval1, urInterval)
    }
  }
}

