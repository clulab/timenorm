package org.clulab.timenorm.scate

import java.time.{DayOfWeek, LocalDateTime, Month}
import java.time.temporal.{ChronoField, ChronoUnit, IsoFields, WeekFields}

import org.clulab.anafora.{Data, Entity, Properties}
import org.clulab.time._

object AnaforaReader {

  class Exception(message: String) extends java.lang.Exception(message)

  private object SomeChronoField {
    def unapply(name: String): Option[ChronoField] =
      try {
        Some(ChronoField.valueOf(name.replace('-', '_').toUpperCase()))
      } catch {
        case _: IllegalArgumentException => None
      }
  }

  private object SomeChronoUnit {
    def unapply(name: String): Option[ChronoUnit] =
      try {
        Some(ChronoUnit.valueOf(name.toUpperCase()))
      } catch {
        case _: IllegalArgumentException => None
      }
  }

  private object SomePluralChronoUnit {
    def unapply(name: String): Option[ChronoUnit] =
      try {
        Some(ChronoUnit.valueOf(name.toUpperCase() + "S"))
      } catch {
        case _: IllegalArgumentException => None
      }
  }
}

class AnaforaReader(val DCT: Interval)(implicit data: Data) {

  private def longValue(entity: Entity): Long = {
    entity.properties.get("Value").filter(_.nonEmpty).filter(_.forall(_.isDigit)).map(_.toLong).getOrElse {
      throw new AnaforaReader.Exception(s"expected numeric Value, found ${entity.xml}")
    }
  }

  def number(entity: Entity)(implicit data: Data): Number = entity.properties("Value") match {
    case "?" => VagueNumber(entity.text.getOrElse(""), Some(entity.fullSpan))
    case "" => throw new AnaforaReader.Exception(s"""cannot parse number from "${entity.text} and ${entity.xml}""")
    case value =>
      if (value.matches("""\d*[.]\d+""")) {
        val (beforeDot, dotAndAfter) = value.span(_ != '.')
        val number = if (beforeDot.isEmpty) 0 else beforeDot.toInt
        val numerator = dotAndAfter.tail.toInt
        val denominator = math.pow(10, dotAndAfter.length - 1).toInt
        val g = gcd(numerator, denominator)
        FractionalNumber(number, numerator / g, denominator / g, Some(entity.fullSpan))
      } else if (value.forall(_.isDigit)) {
        IntNumber(value.toInt, Some(entity.fullSpan))
      } else {
        VagueNumber(value, Some(entity.fullSpan))
      }
  }

  @scala.annotation.tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def integer(entityOption: Option[Entity])(implicit data: Data): Int = entityOption match {
    case None => 1
    case Some(entity) => number(entity) match {
      case IntNumber(number, _) => number
      case _ => throw new AnaforaReader.Exception(
        s"""cannot parse integer from "${entity.text}" and ${entity.descendants.map(_.xml)}""")
    }
  }

  def modifier(entity: Entity)(implicit data: Data): Modifier = entity.properties("Type") match {
    case "Approx" => Modifier.Approx(Some(entity.fullSpan))
    case "Less-Than" => Modifier.LessThan(Some(entity.fullSpan))
    case "More-Than" => Modifier.MoreThan(Some(entity.fullSpan))
    case "Start" => Modifier.Start(Some(entity.fullSpan))
    case "Mid" => Modifier.Mid(Some(entity.fullSpan))
    case "End" => Modifier.End(Some(entity.fullSpan))
    case "Fiscal" => Modifier.Fiscal(Some(entity.fullSpan))
  }

  def modifier(properties: Properties)(implicit data: Data): Option[Modifier] = properties.getEntity("Modifier").map(modifier)

  def period(entity: Entity)(implicit data: Data): Period = {
    val mod = modifier(entity.properties)
    entity.`type` match {
      case "Period" => entity.properties("Type") match {
        case "Unknown" =>
          if (entity.properties.get("Number").exists(_.nonEmpty)) {
            throw new AnaforaReader.Exception(s"expected empty Number, found ${entity.xml}")
          }
          if (entity.properties.get("Modifier").exists(_.nonEmpty)) {
            throw new AnaforaReader.Exception(s"expected empty Modifier, found ${entity.xml}")
          }
          UnknownPeriod(Some(entity.fullSpan))
        case AnaforaReader.SomeChronoUnit(unit) =>
          val n: Number = entity.properties.getEntity("Number") match {
            case Some(numberEntity) => number(numberEntity)
            case None => if (entity.text.exists(_.last == 's')) VagueNumber("2+") else IntNumber(1)
          }
          SimplePeriod(unit, n, mod, Some(entity.fullSpan))
      }
      case "Sum" => SumP(entity.properties.getEntities("Periods").map(period).toSet, mod, Some(entity.fullSpan))
    }
  }

  def included(properties: Properties, prefix: String = ""): Boolean =
    properties.get(prefix + "Included") match {
      case Some("Included") => true
      case Some("Not-Included") => false
      case None => false
      case _ => throw new AnaforaReader.Exception(
        s"""cannot parse ${prefix}Included from ${properties.xml}""")
    }

  def interval(properties: Properties, prefix: String = "")(implicit data: Data): Interval =
    properties(prefix + "Interval-Type") match {
      case "Link" => interval(properties.entity(prefix + "Interval"))
      case "DocTime" => DCT
      case "DocTime-Year" => SimpleInterval.of(DCT.start.getYear)
      case "DocTime-Era" => SimpleInterval(LocalDateTime.of(0, 1, 1, 0, 0), LocalDateTime.MAX)
      case "Unknown" => UnknownInterval()
      case _ => throw new AnaforaReader.Exception(
        s"""cannot parse ${prefix}Interval-Type from ${properties.xml}""")
    }

  def interval(entity: Entity)(implicit data: Data): Interval = {
    val properties = entity.properties
    val valueOption = entity.properties.get("Value")
    val periodEntities = entity.properties.getEntities("Period")
    val periods = periodEntities.map(period)
    val repeatingIntervalEntities =
      entity.properties.getEntities("Repeating-Interval") ++ entity.properties.getEntities("Repeating-Intervals")
    val repeatingIntervals = repeatingIntervalEntities.map(repeatingInterval)
    val numberEntities = repeatingIntervalEntities.map(_.properties.getEntity("Number"))
    val numbers = numberEntities.flatten.map(number)
    val semantics = properties.get("Semantics")
    val N = Seq()
    val Exc = Some("Interval-Not-Included")
    val Inc = Some("Interval-Included")
    val charSpan = Some(entity.fullSpan)

    val result = (entity.`type`, valueOption, periods, repeatingIntervals, numbers, semantics) match {
      case ("Event", None, N, N, N, None) => Event(entity.text.getOrElse(""), charSpan)
      case ("Year", Some(value), N, N, N, None) => value.partition(_ != '?') match {
        case (year, questionMarks) => Year(year.toInt, questionMarks.length, charSpan)
      }
      case ("Two-Digit-Year", Some(value), N, N, N, None) => value.partition(_ != '?') match {
        case (year, questionMarks) => YearSuffix(interval(properties), year.toInt, year.length, questionMarks.length, charSpan)
      }
      case ("Between", None, N, N, N, None) => Between(
        interval(properties, "Start-"),
        interval(properties, "End-"),
        included(properties, "Start-"),
        included(properties, "End-"),
        charSpan)
      case ("This", None, N, N, N, None) => ThisP(interval(properties), UnknownPeriod(), charSpan)
      case ("This", None, Seq(period), N, N, None) => ThisP(interval(properties), period, charSpan)
      case ("This", None, N, Seq(rInterval), N, None) => ThisRI(interval(properties), rInterval, charSpan)
      case ("Last", None, N, N, N, _) => LastP(interval(properties), UnknownPeriod(), charSpan)
      case ("Last", None, Seq(period), N, N, _) => LastP(interval(properties), period, charSpan)
      case ("Last", None, N, Seq(rInterval), N, Exc) => LastRI(interval(properties), rInterval, triggerCharSpan = charSpan)
      case ("Last", None, N, Seq(rInterval), N, Inc) => LastRI(interval(properties), rInterval, from = Interval.End, triggerCharSpan = charSpan)
      case ("Next", None, N, N, N, _) => NextP(interval(properties), UnknownPeriod(), charSpan)
      case ("Next", None, Seq(period), N, N, _) => NextP(interval(properties), period, charSpan)
      case ("Next", None, N, Seq(rInterval), N, Exc) => NextRI(interval(properties), rInterval, triggerCharSpan = charSpan)
      case ("Next", None, N, Seq(rInterval), N, Inc) => NextRI(interval(properties), rInterval, from = Interval.Start, triggerCharSpan = charSpan)
      case ("Before", None, N, N, N, _) => BeforeP(interval(properties), UnknownPeriod(), charSpan)
      case ("Before", None, Seq(period), N, N, _) => BeforeP(interval(properties), period, charSpan)
      case ("Before", None, N, Seq(rInterval), N, Exc) => BeforeRI(interval(properties), rInterval, triggerCharSpan = charSpan)
      case ("Before", None, N, Seq(rInterval), N, Inc) => BeforeRI(interval(properties), rInterval, from = Interval.End, triggerCharSpan = charSpan)
      case ("Before", None, N, Seq(rInterval), Seq(number), Exc) => BeforeRI(interval(properties), rInterval, number, triggerCharSpan = charSpan)
      case ("Before", None, N, Seq(rInterval), Seq(number), Inc) => BeforeRI(interval(properties), rInterval, number, from = Interval.End, triggerCharSpan = charSpan)
      case ("After", None, N, N, N, _) => AfterP(interval(properties), UnknownPeriod(), charSpan)
      case ("After", None, Seq(period), N, N, _) => AfterP(interval(properties), period, charSpan)
      case ("After", None, N, Seq(rInterval), N, Exc) => AfterRI(interval(properties), rInterval, triggerCharSpan = charSpan)
      case ("After", None, N, Seq(rInterval), N, Inc) => AfterRI(interval(properties), rInterval, from = Interval.Start, triggerCharSpan = charSpan)
      case ("After", None, N, Seq(rInterval), Seq(number), Exc) => AfterRI(interval(properties), rInterval, number, triggerCharSpan = charSpan)
      case ("After", None, N, Seq(rInterval), Seq(number), Inc) => AfterRI(interval(properties), rInterval, number, from = Interval.Start, triggerCharSpan = charSpan)
      case ("NthFromStart", Some(value), N, N, N, None) => NthP(interval(properties), value.toInt, UnknownPeriod(), triggerCharSpan = charSpan)
      case ("NthFromStart", Some(value), Seq(period), N, N, None) => NthP(interval(properties), value.toInt, period, triggerCharSpan = charSpan)
      case ("NthFromStart", Some(value), N, Seq(rInterval), N, None) => NthRI(interval(properties), value.toInt, rInterval, triggerCharSpan = charSpan)
      case ("Intersection", None, N, N, N, None) => IntersectionI(entity.properties.getEntities("Intervals").map(interval), charSpan)
      case _ => throw new AnaforaReader.Exception(
        s"""cannot parse Interval from "${entity.text}" and ${entity.descendants.map(_.xml)}""")
    }
    properties.getEntity("Sub-Interval") match {
      case None => result
      case Some(subEntity) => ThisRI(result, repeatingInterval(subEntity))
    }
  }

  def intervals(entity: Entity)(implicit data: Data): Intervals = {
    val valueOption = entity.properties.get("Value")
    val periodEntities = entity.properties.getEntities("Period")
    val periods = periodEntities.map(period)
    val repeatingIntervalEntities =
      entity.properties.getEntities("Repeating-Interval") ++ entity.properties.getEntities("Repeating-Intervals")
    val repeatingIntervals = repeatingIntervalEntities.map(repeatingInterval)
    val numberEntities = repeatingIntervalEntities.map(_.properties.getEntity("Number"))
    val numbers = numberEntities.flatten.map(number)
    val intervalEntities = entity.properties.getEntities("Intervals")
    val intervals = intervalEntities.map(interval)
    val N = Seq()
    val charSpan = Some(entity.fullSpan)

    (entity.`type`, valueOption, periods, repeatingIntervals, numbers, intervals) match {
      case ("Intersection", None, N, Seq(repeatingInterval), N, Seq(interval)) => ThisRIs(interval, repeatingInterval, charSpan)
      case ("Intersection", None, N, ris, N, Seq(interval)) => ThisRIs(interval, IntersectionRI(ris.toSet), charSpan)
      case ("Last", None, N, Seq(rInterval), Seq(number), N) => LastRIs(interval(entity.properties), rInterval, number, triggerCharSpan = charSpan)
      case ("Next", None, N, Seq(rInterval), Seq(number), N) => NextRIs(interval(entity.properties), rInterval, number, triggerCharSpan = charSpan)
      case ("NthFromStart", Some(value), N, Seq(rInterval), Seq(number), N) =>
        NthRIs(interval(entity.properties), value.toInt, rInterval, number, triggerCharSpan = charSpan)
      case _ => throw new AnaforaReader.Exception(
        s"""cannot parse Intervals from "${entity.text}" and ${entity.descendants.map(_.xml)}""")
    }
  }

  def repeatingInterval(entity: Entity)(implicit data: Data): RepeatingInterval = {
    val mod = modifier(entity.properties)
    val charSpan = Some(entity.fullSpan)
    val result = (entity.`type`, entity.properties.get("Type")) match {
      case (_, Some("Unknown")) => UnknownRepeatingInterval(charSpan)
      case ("Union", None) =>
        val repeatingIntervalEntities = entity.properties.getEntities("Repeating-Intervals")
        UnionRI(repeatingIntervalEntities.map(repeatingInterval).toSet, charSpan)
      case ("Intersection", None) =>
        val repeatingIntervals = entity.properties.getEntities("Repeating-Intervals").map(repeatingInterval)
        if (entity.properties.has("Intervals")) throw new AnaforaReader.Exception(
          s"""cannot parse Intersection from "${entity.text}" and ${entity.descendants.map(_.xml)}""")
        IntersectionRI(repeatingIntervals.toSet, charSpan)
      case ("Calendar-Interval" , Some("Century")) => RepeatingUnit(ChronoUnit.CENTURIES, mod, charSpan)
      case ("Calendar-Interval" , Some("Quarter-Century")) => RepeatingUnit(QUARTER_CENTURIES, mod, charSpan)
      case ("Calendar-Interval" , Some("Quarter-Year")) => RepeatingUnit(IsoFields.QUARTER_YEARS, mod, charSpan)
      case ("Calendar-Interval" , Some(AnaforaReader.SomePluralChronoUnit(unit))) => RepeatingUnit(unit, mod, charSpan)
      case ("Week-Of-Year", None) => RepeatingField(WeekFields.ISO.weekOfYear(), longValue(entity), mod, charSpan)
      case ("Season-Of-Year", Some("Spring")) => RepeatingField(SPRING_OF_YEAR, 1L, mod, charSpan)
      case ("Season-Of-Year", Some("Summer")) => RepeatingField(SUMMER_OF_YEAR, 1L, mod, charSpan)
      case ("Season-Of-Year", Some("Fall")) => RepeatingField(FALL_OF_YEAR, 1L, mod, charSpan)
      case ("Season-Of-Year", Some("Winter")) => RepeatingField(WINTER_OF_YEAR, 1L, mod, charSpan)
      case ("Part-Of-Week", Some("Weekend")) => RepeatingField(WEEKEND_OF_WEEK, 1, mod, charSpan)
      case ("Part-Of-Week", Some("Weekdays")) => RepeatingField(WEEKEND_OF_WEEK, 0, mod, charSpan)
      case ("Part-Of-Day", Some("Dawn")) => RepeatingField(ChronoField.SECOND_OF_DAY, 5L * 60L * 60L, Some(Modifier.Approx()), charSpan)
      case ("Part-Of-Day", Some("Morning")) => RepeatingField(MORNING_OF_DAY, 1, mod, charSpan)
      case ("Part-Of-Day", Some("Noon")) => RepeatingField(ChronoField.MINUTE_OF_DAY, 12L * 60L, mod, charSpan)
      case ("Part-Of-Day", Some("Afternoon")) => RepeatingField(AFTERNOON_OF_DAY, 1, mod, charSpan)
      case ("Part-Of-Day", Some("Evening")) => RepeatingField(EVENING_OF_DAY, 1, mod, charSpan)
      case ("Part-Of-Day", Some("Dusk")) => RepeatingField(ChronoField.SECOND_OF_DAY, 19L * 60L * 60L, Some(Modifier.Approx()), charSpan)
      case ("Part-Of-Day", Some("Night")) => RepeatingField(NIGHT_OF_DAY, 1, mod, charSpan)
      case ("Part-Of-Day", Some("Midnight")) => RepeatingField(ChronoField.SECOND_OF_DAY, 0L, mod, charSpan)
      case ("Hour-Of-Day", None) =>
        // TODO: handle time zone
        val value = longValue(entity)
        entity.properties.getEntity("AMPM-Of-Day") match {
          case Some(ampmEntity) => IntersectionRI(Set(
            RepeatingField(ChronoField.CLOCK_HOUR_OF_AMPM, value, mod, charSpan),
            repeatingInterval(ampmEntity)))
          case None => RepeatingField(ChronoField.CLOCK_HOUR_OF_DAY, value, mod, charSpan)
        }
      case (AnaforaReader.SomeChronoField(field), _) =>
        val value: Long = field match {
          case ChronoField.MONTH_OF_YEAR => Month.valueOf(entity.properties("Type").toUpperCase()).getValue
          case ChronoField.DAY_OF_WEEK => DayOfWeek.valueOf(entity.properties("Type").toUpperCase()).getValue
          case ChronoField.AMPM_OF_DAY => entity.properties("Type") match {
            case "AM" => 0L
            case "PM" => 1L
          }
          case ChronoField.DAY_OF_MONTH | ChronoField.MINUTE_OF_HOUR | ChronoField.SECOND_OF_MINUTE => longValue(entity)
          case _ => throw new AnaforaReader.Exception(
            s"""cannot parse ChronoField value from "${entity.text}" and ${entity.descendants.map(_.xml)}""")
        }
        RepeatingField(field, value, mod, charSpan)
      case _ => throw new AnaforaReader.Exception(
        s"""cannot parse RepeatingInterval from "${entity.text}" and ${entity.descendants.map(_.xml)}""")
    }
    flatten(entity.properties.getEntities("Sub-Interval") match {
      case Seq() => result
      //case subEntities => IntersectionRI(Set(result) ++ subEntities.map(repeatingInterval))
      case subEntities => IntersectionRI(Set(result) ++ Set(repeatingInterval(subEntities.head))) // Only take the first SubInterval. If there are more than one they should be equivalent.

    })
  }

  def flatten(repeatingInterval: RepeatingInterval): RepeatingInterval = repeatingInterval match {
    case IntersectionRI(repeatingIntervals, charSpan) => IntersectionRI(repeatingIntervals.flatMap {
      case IntersectionRI(subIntervals, _) => subIntervals.map(flatten)
      case rIntervals => Set(rIntervals)
    }, charSpan)
    case other => other
  }

  def temporal(entity: Entity)(implicit data: Data): TimeExpression = {
    val intervalEntities = entity.properties.getEntities("Intervals")
    val repeatingIntervalEntities =
      entity.properties.getEntities("Repeating-Interval") ++ entity.properties.getEntities("Repeating-Intervals")
    entity.`type` match {
      case "Number" => number(entity)
      case "Modifier" => modifier(entity)
      case "Period" | "Sum" => period(entity)
      case "Intersection" if intervalEntities.size > 1 && repeatingIntervalEntities.isEmpty => interval(entity)
      case "Intersection" if intervalEntities.size == 1 && repeatingIntervalEntities.nonEmpty => intervals(entity)
      case "Event" | "Year" | "Two-Digit-Year" | "Between" | "This" | "Before" | "After" => interval(entity)
      case "Last" | "Next" | "NthFromStart" =>
        repeatingIntervalEntities.flatMap(_.properties.getEntity("Number")).map(number) match {
          case Seq() => interval(entity)
          case Seq(_) => intervals(entity)
        }
      case "Time-Zone" => TimeZone(entity.text.getOrElse(""))
      case _ => repeatingInterval(entity)
    }
  }
}
