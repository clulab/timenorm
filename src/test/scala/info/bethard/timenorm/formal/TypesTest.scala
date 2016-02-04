package info.bethard.timenorm.formal

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class TypesTest extends FunSuite {
  
  test( "Year Type" ) {
    val y = 1985
    val year = Year( y )
    
    assert( year.min.getYear() == y )
    assert( year.min.getMonthValue() == 1 )
    assert( year.min.getDayOfYear() == 1 )
    assert( year.min.getHour() == 0 )
    assert( year.min.getMinute() == 0 )
    assert( year.min.getSecond() == 0 )
    assert( year.min.getNano() == 0 )
    
    assert( year.max.getYear() == y + 1 )
    assert( year.max.getMonthValue() == 1 )
    assert( year.max.getDayOfYear() == 1 )
    assert( year.max.getHour() == 0 )
    assert( year.max.getMinute() == 0 )
    assert( year.max.getSecond() == 0 )
    assert( year.max.getNano() == 0 )
  }
  
  test( "Decade Type" ) {
    val decade = Decade( 1982 )
    
    assert( decade.min.getYear() == 1980 )
    assert( decade.max.getYear() == 1990 )
  }
  
  test( "Century Type" ) {
    val century = Century( 1776 )
    
    assert( century.min.getYear() == 1700 )
    assert( century.max.getYear() == 1800 )
  }
  
  test( "TwoDigitYear Type" ) {
    val fromYear = TwoDigitYear( Year( 1903 ), 37 )
    assert( fromYear.min.getYear() == 1937 )
    assert( fromYear.max.getYear() == 1938 )
    
    val fromDecade = TwoDigitYear( Decade( 1324 ), 85 )
    assert( fromDecade.min.getYear() == 1385 )
    assert( fromDecade.max.getYear() == 1386 )
    
    val fromCentury = TwoDigitYear( Century( 2345 ), 22 )
    assert( fromCentury.min.getYear() == 2322 )
    assert( fromCentury.max.getYear() == 2323 )
    
  }
  
}