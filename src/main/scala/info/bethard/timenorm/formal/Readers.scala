package info.bethard.timenorm.formal

import java.time.{DayOfWeek, Month}
import java.time.temporal.{WeekFields, ChronoField, ChronoUnit}

import info.bethard.anafora.{Properties, Data, Entity}
import info.bethard.timenorm.field.{NIGHT_OF_DAY, EVENING_OF_DAY, AFTERNOON_OF_DAY, MORNING_OF_DAY}

object AnaforaReader {

  def number(entity: Entity)(implicit data: Data): Number = entity.properties("Value") match {
    case "?" => VagueNumber(entity.text)
    case value => if (!value.contains(".")) {
      IntNumber(value.toInt)
    } else {
      val doubleValue = value.toDouble
      val number = doubleValue.toInt
      // only handle /2 case
      if (doubleValue - number != 0.5) ???
      else FractionalNumber(number, 1, 2)
    }
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

  def period(entity: Entity)(implicit data: Data): Period = {
    val mod = modifier(entity.properties)
    entity.`type` match {
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
          mod)
      }
      case "Sum" => PeriodSum(entity.properties.getEntities("Periods").map(period).toSet, mod)
    }
  }

  def interval(properties: Properties, prefix: String = "")(implicit data: Data): Interval =
    properties(prefix + "Interval-Type") match {
      case "Link" => interval(properties.entity(prefix + "Interval"))
      case "DocTime" => DocumentCreationTime
      case "Unknown" => UnknownInterval
    }

  def interval(entity: Entity)(implicit data: Data): Interval = entity.`type` match {
    case "Year" =>
      entity.properties("Value").partition(_ != '?') match {
        case (year, "") => Year(year.toInt)
        case (decade, "?") => Decade(decade.toInt)
        case (century, "??") => Century(century.toInt)
      }
      // TODO: handle sub-interval for Year and Two-Digit-Year
    case "Two-Digit-Year" => TwoDigitYear(interval(entity.properties), entity.properties("Value").toInt)
    case "This" => interval(entity, ThisPeriod, ThisRepeatingInterval)
    case "Last" => interval(entity, LastPeriod, LastRepeatingInterval)
    case "Next" => interval(entity, NextPeriod, NextRepeatingInterval)
    case "Before" => interval(entity, BeforePeriod, BeforeRepeatingInterval)
    case "After" => interval(entity, AfterPeriod, AfterRepeatingInterval)
    case "Between" => Between(interval(entity.properties, "Start-"), interval(entity.properties, "End-"))
    case "Nth" => Nth(
      interval(entity.properties),
      entity.properties("Value").toInt,
      repeatingInterval(entity.properties.entity("Repeating-Interval")))
    case "Event" => Event(entity.text)
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

  def repeatingInterval(entity: Entity)(implicit data: Data): RepeatingInterval = {
    // TODO: handle Sub-Interval
    val mod = modifier(entity.properties)
    entity.`type` match {
      case "Union" =>
        val repeatingIntervalEntities = entity.properties.getEntities("Repeating-Intervals")
        RepeatingIntervalUnion(repeatingIntervalEntities.map(repeatingInterval).toSet)
      case "Intersection" =>
        val repeatingIntervalEntities = entity.properties.getEntities("Repeating-Intervals")
        val intervalEntities = entity.properties.getEntities("Intervals")
        if (!intervalEntities.isEmpty) ???
        RepeatingIntervalIntersection(repeatingIntervalEntities.map(repeatingInterval).toSet)
      case "Calendar-Interval" => UnitRepeatingInterval(entity.properties("Type") match {
        case "Century" => ChronoUnit.CENTURIES
        case other => ChronoUnit.valueOf(other.toUpperCase + "S")
      }, mod)
      case "Week-Of-Year" => FieldRepeatingInterval(
        WeekFields.ISO.weekOfYear(),
        entity.properties("Value").toLong,
        mod)
      case "Part-Of-Day" => entity.properties("Type") match {
        case "Morning" => FieldRepeatingInterval(MORNING_OF_DAY, 1, mod)
        case "Noon" => FieldRepeatingInterval(ChronoField.SECOND_OF_DAY, 43200L, mod)
        case "Afternoon" => FieldRepeatingInterval(AFTERNOON_OF_DAY, 1, mod)
        case "Evening" => FieldRepeatingInterval(EVENING_OF_DAY, 1, mod)
        case "Night" => FieldRepeatingInterval(NIGHT_OF_DAY, 1, mod)
        case "Midnight" => FieldRepeatingInterval(ChronoField.SECOND_OF_DAY, 0L, mod)
      }
      case "Hour-Of-Day" => {
        // TODO: handle time zone
        val value = entity.properties("Value").toLong
        entity.properties.getEntity("AMPM-Of-Day") match {
          case Some(ampmEntity) => RepeatingIntervalIntersection(Set(
            FieldRepeatingInterval(ChronoField.HOUR_OF_AMPM, value, mod),
            repeatingInterval(ampmEntity)))
          case None => FieldRepeatingInterval(ChronoField.HOUR_OF_DAY, value, mod)
        }
      }
      case name =>
        val field = ChronoField.valueOf(name.replace('-', '_').toUpperCase())
        val value = field match {
          case ChronoField.MONTH_OF_YEAR => Month.valueOf(entity.properties("Type").toUpperCase()).getValue
          case ChronoField.DAY_OF_WEEK => DayOfWeek.valueOf(entity.properties("Type").toUpperCase()).getValue
          case ChronoField.AMPM_OF_DAY => entity.properties("Type") match {
            case "AM" => 0L
            case "PM" => 1L
          }
          case ChronoField.DAY_OF_MONTH | ChronoField.MINUTE_OF_HOUR | ChronoField.SECOND_OF_MINUTE =>
            entity.properties("Value").toLong
        }
        FieldRepeatingInterval(field, value, mod)
    }
  }

  def temporal(entity: Entity)(implicit data: Data): Temporal = entity.`type` match {
    case "Number" => number(entity)
    case "Modifier" => modifier(entity)
    case "Period" | "Sum" => period(entity)
    case "Year" | "Two-Digit-Year" | "This" | "Last" | "Next" | "Before" | "After" | "Between" | "Nth" | "Event" =>
      interval(entity)
    case "Time-Zone" => TimeZone(entity.text)
    case _ => repeatingInterval(entity)
  }
}