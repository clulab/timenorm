package info.bethard.timenorm.formal

import java.time.temporal.{TemporalUnit, UnsupportedTemporalTypeException, ChronoUnit}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.time.LocalDateTime
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
  
}

