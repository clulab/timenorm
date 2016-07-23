package info.bethard.timenorm.formal

import java.time.{DayOfWeek, Month}
import java.time.temporal.{IsoFields, WeekFields, ChronoField, ChronoUnit}

import info.bethard.anafora.{Properties, Data, Entity}
import info.bethard.timenorm.field._

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

  def integer(entityOption: Option[Entity])(implicit data: Data): Int = entityOption match {
    case None => 1
    case Some(entity) => number(entity) match {
      case IntNumber(number) => number
      case _ => ???
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

  def interval(entity: Entity)(implicit data: Data): Interval = {
    val properties = entity.properties
    val result = entity.`type` match {
      case "Year" => properties("Value").partition(_ != '?') match {
        case (year, "") => Year(year.toInt)
        case (decade, "?") => Decade(decade.toInt)
        case (century, "??") => Century(century.toInt)
      }
      case "Event" => Event(entity.text)
      case "Two-Digit-Year" => TwoDigitYear(interval(properties), properties("Value").toInt)
      case "Between" => Between(interval(properties, "Start-"), interval(properties, "End-"))
      case opName => (opName, properties.getEntities("Period"), properties.getEntities("Repeating-Interval")) match {
        case ("This", Seq(), Seq()) => ThisPeriod(interval(properties), UnknownPeriod)
        case ("This", Seq(entity), Seq()) => ThisPeriod(interval(properties), period(entity))
        case ("This", Seq(), Seq(entity)) => ThisRepeatingInterval(interval(properties), repeatingInterval(entity))
        case ("Last", Seq(), Seq()) => LastPeriod(interval(properties), UnknownPeriod)
        case ("Last", Seq(entity), Seq()) => LastPeriod(interval(properties), period(entity))
        case ("Last", Seq(), Seq(entity)) => LastRepeatingInterval(interval(properties), repeatingInterval(entity))
        case ("Next", Seq(), Seq()) => NextPeriod(interval(properties), UnknownPeriod)
        case ("Next", Seq(entity), Seq()) => NextPeriod(interval(properties), period(entity))
        case ("Next", Seq(), Seq(entity)) => NextRepeatingInterval(interval(properties), repeatingInterval(entity))
        case ("Before", Seq(), Seq()) => BeforePeriod(interval(properties), UnknownPeriod)
        case ("Before", Seq(entity), Seq()) => BeforePeriod(interval(properties), period(entity))
        case ("Before", Seq(), Seq(entity)) => BeforeRepeatingInterval(interval(properties), repeatingInterval(entity), integer(entity.properties.getEntity("Number")))
        case ("After", Seq(), Seq()) => AfterPeriod(interval(properties), UnknownPeriod)
        case ("After", Seq(entity), Seq()) => AfterPeriod(interval(properties), period(entity))
        case ("After", Seq(), Seq(entity)) => AfterRepeatingInterval(interval(properties), repeatingInterval(entity), integer(entity.properties.getEntity("Number")))
        case ("This" | "Last" | "Next" | "Before" | "After", _, _) =>
          assert(false, s"expected exactly one Period or Repeating-Interval, found ${entity.xml}")
          ???
        case ("Nth", Seq(), Seq(entity)) => Nth(interval(properties), properties("Value").toInt, repeatingInterval(entity))
        case ("Nth", _, _) =>
          assert(false, s"expected exactly one Repeating-Interval, found ${entity.xml}")
          ???
      }
    }
    properties.getEntity("Sub-Interval") match {
      case None => result
      case Some(subEntity) => ThisRepeatingInterval(result, repeatingInterval(subEntity))
    }
  }

  def repeatingInterval(entity: Entity)(implicit data: Data): RepeatingInterval = {
    val mod = modifier(entity.properties)
    val result = entity.`type` match {
      case "Union" =>
        val repeatingIntervalEntities = entity.properties.getEntities("Repeating-Intervals")
        RepeatingIntervalUnion(repeatingIntervalEntities.map(repeatingInterval).toSet)
      case "Intersection" =>
        val repeatingIntervals = entity.properties.getEntities("Repeating-Intervals").map(repeatingInterval)
        // TODO: handle Intersection intervals
        if (entity.properties.has("Intervals")) ???
        RepeatingIntervalIntersection(repeatingIntervals.toSet)
      case "Calendar-Interval" => UnitRepeatingInterval(entity.properties("Type") match {
        case "Century" => ChronoUnit.CENTURIES
        case "Quarter-Year" => IsoFields.QUARTER_YEARS
        case other => ChronoUnit.valueOf(other.toUpperCase + "S")
      }, mod)
      case "Week-Of-Year" => FieldRepeatingInterval(
        WeekFields.ISO.weekOfYear(),
        entity.properties("Value").toLong,
        mod)
      case "Season-Of-Year" => FieldRepeatingInterval(entity.properties("Type") match {
        case "Spring" => SPRING_OF_YEAR
        case "Summer" => SUMMER_OF_YEAR
        case "Fall" => FALL_OF_YEAR
        case "Winter" => WINTER_OF_YEAR
      }, 1L, mod)
      case "Part-Of-Day" => entity.properties("Type") match {
        case "Morning" => FieldRepeatingInterval(MORNING_OF_DAY, 1, mod)
        case "Noon" => FieldRepeatingInterval(ChronoField.SECOND_OF_DAY, 43200L, mod)
        case "Afternoon" => FieldRepeatingInterval(AFTERNOON_OF_DAY, 1, mod)
        case "Evening" => FieldRepeatingInterval(EVENING_OF_DAY, 1, mod)
        case "Night" => FieldRepeatingInterval(NIGHT_OF_DAY, 1, mod)
        case "Midnight" => FieldRepeatingInterval(ChronoField.SECOND_OF_DAY, 0L, mod)
      }
      case "Hour-Of-Day" =>
        // TODO: handle time zone
        val value = entity.properties("Value").toLong
        entity.properties.getEntity("AMPM-Of-Day") match {
          case Some(ampmEntity) => RepeatingIntervalIntersection(Set(
            FieldRepeatingInterval(ChronoField.HOUR_OF_AMPM, value, mod),
            repeatingInterval(ampmEntity)))
          case None => FieldRepeatingInterval(ChronoField.HOUR_OF_DAY, value, mod)
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
    entity.properties.getEntity("Sub-Interval") match {
      case None => result
      case Some(subEntity) => RepeatingIntervalIntersection(Set(result, repeatingInterval(subEntity)))
    }
  }

  def temporal(entity: Entity)(implicit data: Data): TimeExpression = entity.`type` match {
    case "Number" => number(entity)
    case "Modifier" => modifier(entity)
    case "Period" | "Sum" => period(entity)
    // TODO: handle Seq[Interval] operators for "This", "Last", "Next"
    case "Year" | "Two-Digit-Year" | "This" | "Last" | "Next" | "Before" | "After" | "Between" | "Nth" | "Event" =>
      interval(entity)
    case "Time-Zone" => TimeZone(entity.text)
    case _ => repeatingInterval(entity)
  }
}