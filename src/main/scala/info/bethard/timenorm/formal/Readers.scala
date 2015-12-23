package info.bethard.timenorm.formal

import java.time.Month
import java.time.temporal.{WeekFields, ChronoField, ChronoUnit}

import info.bethard.anafora.{Properties, Data, Entity}

object AnaforaReader {

  def number(entity: Entity)(implicit data: Data): Number = entity.properties("Value") match {
    case "?" => VagueNumber(entity.text)
    case value => IntNumber(value.toInt)
  }

  def modifier(entity: Entity)(implicit data: Data): Modifier = entity.properties("Type") match {
    case "Approx" => Modifier.Approx
    case "Less-Than" => Modifier.LessThan
    case "More-Than" => Modifier.MoreThan
    case "Start" => Modifier.Start
    case "Mid" => Modifier.Mid
    case "End" => Modifier.End
    case "Fiscal" => Modifier.Fiscal
  }

  def modifier(properties: Properties)(implicit data: Data): Modifier = properties.getEntity("Modifier") match {
    case None => Modifier.Exact
    case Some(modifierEntity) => modifier(modifierEntity)
  }

  def period(entity: Entity)(implicit data: Data): Period = entity.`type` match {
    case "Period" => entity.properties("Type") match {
      case "Unknown" =>
        assert(!entity.properties.has("Number"), s"expected empty Number, found ${entity.xml}")
        assert(!entity.properties.has("Modifier"), s"expected empty Modifier, found ${entity.xml}")
        UnknownPeriod
      case _ => SimplePeriod(
        ChronoUnit.valueOf(entity.properties("Type").toUpperCase()),
        entity.properties.getEntity("Number") match {
          case Some(numberEntity) => number(numberEntity)
          case None => if (entity.text.last != 's') IntNumber(1) else VagueNumber("2+")
        },
        modifier(entity.properties))
    }
  }

  def interval(properties: Properties)(implicit data: Data): Interval = properties("Interval-Type") match {
    case "Link" => interval(properties.entity("Interval"))
    case "DocTime" => DocumentCreationTime
    case "Unknown" => UnknownInterval
  }

  def interval(entity: Entity)(implicit data: Data): Interval = entity.`type` match {
    case "Year" => Year(entity.properties("Value").toInt)
    case "This" => interval(entity, ThisPeriod, ThisRepeatingInterval)
    case "Last" => interval(entity, LastPeriod, LastRepeatingInterval)
    case "Next" => interval(entity, NextPeriod, NextRepeatingInterval)
    case "Before" => interval(entity, BeforePeriod, BeforeRepeatingInterval)
    case "After" => interval(entity, AfterPeriod, AfterRepeatingInterval)
    case "Event" => Event
  }

  private def interval(entity: Entity,
                       periodFunc: (Interval, Period) => Interval,
                       repeatingIntervalFunc: (Interval, RepeatingInterval) => Interval)(implicit data: Data): Interval = {
    entity.properties.getEntity("Repeating-Interval") match {
      case Some(repeatingIntervalEntity) =>
        assert(!entity.properties.has("Period"),
          s"expected empty Period, found ${entity.xml}")
        repeatingIntervalFunc(interval(entity.properties), repeatingInterval(repeatingIntervalEntity))
      case None => entity.properties.getEntity("Period") match {
        case Some(periodEntity) => periodFunc(interval(entity.properties), period(periodEntity))
        case None => periodFunc(interval(entity.properties), UnknownPeriod)
      }
    }
  }

  def repeatingInterval(entity: Entity)(implicit data: Data): RepeatingInterval = entity.`type` match {
    case "Calendar-Interval" => UnitRepeatingInterval(
      ChronoUnit.valueOf(entity.properties("Type").toUpperCase + "S"),
      modifier(entity.properties))
    case "Week-Of-Year" => FieldRepeatingInterval(
      WeekFields.ISO.weekOfYear(),
      entity.properties("Value").toLong,
      modifier(entity.properties))
    case name =>
      val field = ChronoField.valueOf(name.replace('-', '_').toUpperCase())
      val value = field match {
        case ChronoField.MONTH_OF_YEAR => Month.valueOf(entity.properties("Type").toUpperCase()).getValue
        case ChronoField.DAY_OF_MONTH => entity.properties("Value").toLong
      }
      FieldRepeatingInterval(field, value, modifier(entity.properties))
  }

  def temporal(entity: Entity)(implicit data: Data): Temporal = entity.`type` match {
    case "Number" => number(entity)
    case "Modifier" => modifier(entity)
    case "Period" => period(entity)
    case "Year" | "This" | "Last" | "Next" | "Before" | "After" | "Event" => interval(entity)
    case _ => repeatingInterval(entity)
  }
}