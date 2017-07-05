package info.bethard.timenorm.formal

import java.time.{DayOfWeek, Month, LocalDateTime}
import java.time.temporal.{IsoFields, WeekFields, ChronoField, ChronoUnit}

import info.bethard.anafora.{Properties, Data, Entity}
import info.bethard.timenorm.field._


//object AnaforaReader {
class AnaforaReader(dct: LocalDateTime)(implicit data: Data) {
  val DCT: SimpleInterval = SimpleInterval(dct, dct.plusDays(1))

  def number(entity: Entity)(implicit data: Data): Number = entity.properties("Value") match {
    case "?" => VagueNumber(entity.text)
    case value =>
      if (value.contains(".")) {
        val (beforeDot, dotAndAfter) = value.span(_ != '.')
        val number = if (beforeDot.isEmpty) 0 else beforeDot.toInt
        val numerator = dotAndAfter.tail.toInt
        val denominator = math.pow(10, dotAndAfter.size - 1).toInt
        val g = gcd(numerator, denominator)
        FractionalNumber(number, numerator / g, denominator / g)
      } else if (value.forall(_.isDigit)) {
        IntNumber(value.toInt)
      } else {
        VagueNumber(value)
      }
  }

  @scala.annotation.tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

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
      case "Sum" => Sum(entity.properties.getEntities("Periods").map(period).toSet, mod)
    }
  }

  def interval(properties: Properties, prefix: String = "")(implicit data: Data): Interval =
    properties(prefix + "Interval-Type") match {
      case "Link" => interval(properties.entity(prefix + "Interval"))
      case "DocTime" => DocumentCreationTime(DCT)
      case "Unknown" => UnknownInterval
    }

  def interval(entity: Entity)(implicit data: Data): Interval = {
    val properties = entity.properties
    val result = entity.`type` match {
      case "Year" => properties("Value").partition(_ != '?') match {
        case (year, questionMarks) => Year(year.toInt, questionMarks.size)
      }
      case "Event" => Event(entity.text)
      case "Two-Digit-Year" => properties("Value").partition(_ != '?') match {
        case (year, questionMarks) => YearSuffix(interval(properties), year.toInt, questionMarks.size)
      }
      case "Between" => Between(interval(properties, "Start-"), interval(properties, "End-"))
      case opName => (opName, properties.getEntities("Period"), properties.getEntities("Repeating-Interval")) match {
        case ("This", Seq(), Seq()) => ThisP(interval(properties), UnknownPeriod)
        case ("This", Seq(entity), Seq()) => ThisP(interval(properties), period(entity))
        case ("This", Seq(), Seq(entity)) => ThisRI(interval(properties), repeatingInterval(entity))
        case ("Last", Seq(), Seq()) => LastP(interval(properties), UnknownPeriod)
        case ("Last", Seq(entity), Seq()) => LastP(interval(properties), period(entity))
        case ("Last", Seq(), Seq(entity)) => LastRI(interval(properties), repeatingInterval(entity))
        case ("Next", Seq(), Seq()) => NextP(interval(properties), UnknownPeriod)
        case ("Next", Seq(entity), Seq()) => NextP(interval(properties), period(entity))
        case ("Next", Seq(), Seq(entity)) => NextRI(interval(properties), repeatingInterval(entity))
        case ("Before", Seq(), Seq()) => BeforeP(interval(properties), UnknownPeriod)
        case ("Before", Seq(entity), Seq()) => BeforeP(interval(properties), period(entity))
        case ("Before", Seq(), Seq(entity)) => BeforeRI(interval(properties), repeatingInterval(entity), integer(entity.properties.getEntity("Number")))
        case ("After", Seq(), Seq()) => AfterP(interval(properties), UnknownPeriod)
        case ("After", Seq(entity), Seq()) => AfterP(interval(properties), period(entity))
        case ("After", Seq(), Seq(entity)) => AfterRI(interval(properties), repeatingInterval(entity), integer(entity.properties.getEntity("Number")))
        case ("NthFromStart", Seq(), Seq()) => NthFromStartP(interval(properties), properties("Value").toInt, UnknownPeriod)
        case ("NthFromStart", Seq(), Seq(entity)) => NthFromStartRI(interval(properties), properties("Value").toInt, repeatingInterval(entity))
        case ("NthFromStart", Seq(entity), Seq()) => NthFromStartP(interval(properties), properties("Value").toInt, period(entity))
        case ("This" | "Last" | "Next" | "Before" | "After" | "NthFromStart", _, _) =>
          assert(false, s"expected one Period or Repeating-Interval, found ${entity.xml}")
          ???
      }
    }
    properties.getEntity("Sub-Interval") match {
      case None => result
      case Some(subEntity) => ThisRI(result, repeatingInterval(subEntity))
    }
  }

  def intervals(entity: Entity)(implicit data: Data): Intervals = {
    entity.`type` match {
      case "Intersection" => entity.properties.getEntities("Intervals").map(interval) match {
        case Seq(interval) => entity.properties.getEntities("Repeating-Intervals").map(repeatingInterval) match {
          case Seq(repeatingInterval) => ThisRIs(interval, repeatingInterval)
          case repeatingIntervals =>
            ThisRIs(interval, Intersection(repeatingIntervals.toSet))
        }
        case _ => ???
      }
    }
  }

  def repeatingInterval(entity: Entity)(implicit data: Data): RepeatingInterval = {
    val mod = modifier(entity.properties)
    val result = entity.`type` match {
      case "Union" =>
        val repeatingIntervalEntities = entity.properties.getEntities("Repeating-Intervals")
        Union(repeatingIntervalEntities.map(repeatingInterval).toSet)
      case "Intersection" =>
        val repeatingIntervals = entity.properties.getEntities("Repeating-Intervals").map(repeatingInterval)
        if (entity.properties.has("Intervals")) ???
        Intersection(repeatingIntervals.toSet)
      case "Calendar-Interval" => RepeatingUnit(entity.properties("Type") match {
        case "Century" => ChronoUnit.CENTURIES
        case "Quarter-Year" => IsoFields.QUARTER_YEARS
        case other => ChronoUnit.valueOf(other.toUpperCase + "S")
      }, mod)
      case "Week-Of-Year" => RepeatingField(
        WeekFields.ISO.weekOfYear(),
        entity.properties("Value").toLong,
        mod)
      case "Season-Of-Year" => RepeatingField(entity.properties("Type") match {
        case "Spring" => SPRING_OF_YEAR
        case "Summer" => SUMMER_OF_YEAR
        case "Fall" => FALL_OF_YEAR
        case "Winter" => WINTER_OF_YEAR
      }, 1L, mod)
      case "Part-Of-Week" => entity.properties("Type") match {
        case "Weekend" => RepeatingField(WEEKEND_OF_WEEK, 1, mod)
        case "Weekdays" => RepeatingField(WEEKEND_OF_WEEK, 0, mod)
      }
      case "Part-Of-Day" => entity.properties("Type") match {
        case "Dawn" => RepeatingField(ChronoField.SECOND_OF_DAY, 5L * 60L * 60L, Modifier.Approx)
        case "Morning" => RepeatingField(MORNING_OF_DAY, 1, mod)
        case "Noon" => RepeatingField(ChronoField.SECOND_OF_DAY, 12L * 60L * 60L, mod)
        case "Afternoon" => RepeatingField(AFTERNOON_OF_DAY, 1, mod)
        case "Evening" => RepeatingField(EVENING_OF_DAY, 1, mod)
        case "Dusk" => RepeatingField(ChronoField.SECOND_OF_DAY, 19L * 60L * 60L, Modifier.Approx)
        case "Night" => RepeatingField(NIGHT_OF_DAY, 1, mod)
        case "Midnight" => RepeatingField(ChronoField.SECOND_OF_DAY, 0L, mod)
      }
      case "Hour-Of-Day" =>
        // TODO: handle time zone
        val value = entity.properties("Value").toLong
        entity.properties.getEntity("AMPM-Of-Day") match {
          case Some(ampmEntity) => Intersection(Set(
            RepeatingField(ChronoField.HOUR_OF_AMPM, value, mod),
            repeatingInterval(ampmEntity)))
          case None => RepeatingField(ChronoField.HOUR_OF_DAY, value, mod)
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
        RepeatingField(field, value, mod)
    }
    flatten(entity.properties.getEntities("Sub-Interval") match {
      case Seq() => result
      //case subEntities => Intersection(Set(result) ++ subEntities.map(repeatingInterval))
      case subEntities => Intersection(Set(result) ++ Set(repeatingInterval(subEntities.head)))
    })
  }

  def flatten(repeatingInterval: RepeatingInterval): RepeatingInterval = repeatingInterval match {
    case Intersection(repeatingIntervals) => Intersection(repeatingIntervals.map{
      case Intersection(subIntervals) => subIntervals.map(flatten)
      case repeatingInterval => Set(repeatingInterval)
    }.flatten)
    case other => other
  }

  def temporal(entity: Entity)(implicit data: Data): TimeExpression = entity.`type` match {
    case "Number" => number(entity)
    case "Modifier" => modifier(entity)
    case "Period" | "Sum" => period(entity)
    case "Intersection" if entity.properties.has("Intervals") => intervals(entity)
    // TODO: handle Seq[Interval] operators for "This", "Last", "Next"
    case "Year" | "Two-Digit-Year" | "This" | "Last" | "Next" | "Before" | "After" | "Between" | "NthFromStart" | "Event" =>
      interval(entity)
    case "Time-Zone" => TimeZone(entity.text)
    case _ => repeatingInterval(entity)
  }
}
