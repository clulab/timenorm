package info.bethard.timenorm.formal

import java.time.Month
import java.time.temporal.{ChronoField, ChronoUnit}

import info.bethard.anafora.{Properties, Data, Entity}

object AnaforaReader {

  def number(entity: Entity): Number = {
    val value = entity.properties("Value")
    if (value.forall(_.isDigit)) {
      IntNumber(value.toInt)
    } else {
      VagueNumber(value)
    }
  }

  def period(entity: Entity)(implicit data: Data): Period = entity.`type` match {
    case "Period" => entity.properties("Type") match {
      case "Unknown" => {
        assert(!entity.properties.has("Number"), s"expected empty Number, found ${entity.xml}")
        assert(!entity.properties.has("Modifier"), s"expected empty Modifier, found ${entity.xml}")
        UnknownPeriod
      }
      case _ => SimplePeriod(
        ChronoUnit.valueOf(entity.properties("Type").toUpperCase()),
        number(entity.properties.entity("Number")),
        entity.properties.get("Modifier") match {
          case None => Modifier.Exact
          case Some(string) => ???
        })
    }
  }

  def interval(properties: Properties)(implicit data: Data): Interval = properties("Interval-Type") match {
    case "DocTime" => DocumentCreationTime
  }

  def interval(entity: Entity)(implicit data: Data): Interval = entity.`type` match {
    case "Last" => interval(entity, LastPeriod, LastRepeatingInterval)
    case "Before" => interval(entity, BeforePeriod, BeforeRepeatingInterval)
  }

  private def interval(entity: Entity,
                       periodFunc: (Interval, Period) => Interval,
                       repeatingIntervalFunc: (Interval, RepeatingInterval) => Interval)(implicit data: Data): Interval = {
    entity.properties.getEntity("Period") match {
      case Some(periodEntity) => {
        assert(!entity.properties.has("Repeating-Interval"),
          s"expected empty Repeating-Interval, found ${entity.xml}")
        periodFunc(interval(entity.properties), period(periodEntity))
      }
      case None => {
        assert(entity.properties.has("Repeating-Interval"),
          s"expected Repeating-Interval, found ${entity.xml}")
        assert(!entity.properties.has("Repeating-Interval-Number"),
          s"expected empty Repeating-Interval-Number, found ${entity.xml}")
        val repeatingIntervalEntity = entity.properties.entity("Repeating-Interval")
        repeatingIntervalFunc(interval(entity.properties), repeatingInterval(repeatingIntervalEntity))
      }
    }
  }

  def repeatingInterval(entity: Entity)(implicit data: Data): RepeatingInterval = entity.`type` match {
    case "Calendar-Interval" => {
      UnitRepeatingInterval(ChronoUnit.valueOf(entity.properties("Type").toUpperCase + "S"))
    }
    case name => {
      val field = ChronoField.valueOf(name.replace('-', '_').toUpperCase())
      val value = field match {
        case ChronoField.MONTH_OF_YEAR => Month.valueOf(entity.properties("Type").toUpperCase()).getValue
      }
      FieldRepeatingInterval(field, value)
    }
  }

  def temporal(entity: Entity)(implicit data: Data): Temporal = entity.`type` match {
    case "Number" => number(entity)
    case "Period" => period(entity)
    case "Last" | "Before" => entity.properties.has("Repeating-Interval-Number") match {
      case false => interval(entity)
      case true => ???
    }
    case _ => repeatingInterval(entity)
  }
}