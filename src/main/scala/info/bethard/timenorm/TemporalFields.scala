package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ValueRange
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.format.DateTimeBuilder

private[timenorm] object DECADE extends TemporalField {
  def getName: String = "Decade"
  def getBaseUnit: TemporalUnit = DECADES
  def getRangeUnit: TemporalUnit = DECADES
  def range: ValueRange = ValueRange.of(-999, +999)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) / 10
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = YEAR.doWith(temporal, newValue * 10)

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object YEAR_OF_DECADE extends TemporalField {
  def getName: String = "YearOfDecade"
  def getBaseUnit: TemporalUnit = YEARS
  def getRangeUnit: TemporalUnit = DECADES
  def range: ValueRange = ValueRange.of(0, 9)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) % 10
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    val oldYear = YEAR.doGet(temporal)
    YEAR.doWith(temporal, oldYear - oldYear % 10L + newValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object CENTURY extends TemporalField {
  def getName: String = "Century"
  def getBaseUnit: TemporalUnit = CENTURIES
  def getRangeUnit: TemporalUnit = CENTURIES
  def range: ValueRange = ValueRange.of(-99, +99)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) / 100
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = YEAR.doWith(temporal, newValue * 100)

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object YEAR_OF_CENTURY extends TemporalField {
  def getName: String = "YearOfCentury"
  def getBaseUnit: TemporalUnit = YEARS
  def getRangeUnit: TemporalUnit = CENTURIES
  def range: ValueRange = ValueRange.of(0, 99)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) % 100
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    val oldYear = YEAR.doGet(temporal)
    YEAR.doWith(temporal, oldYear - oldYear % 100L + newValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}
