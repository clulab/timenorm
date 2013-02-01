package info.bethard.timenorm

import org.threeten.bp.{ Period => JPeriod }
import org.threeten.bp.temporal.{ SimplePeriod => JSimplePeriod }
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.{ Temporal => JTemporal }
import org.threeten.bp.temporal.TemporalAdder
import org.threeten.bp.temporal.TemporalSubtractor
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.ZonedDateTime

sealed trait Temporal

object Temporal {

  def fromParse(parse: SynchronousParser.Parse): Temporal = {
    var nonTerminalIndex = -1
    val targetSeq = for ((token, i) <- parse.rule.targetSeq.zipWithIndex) yield {
      if (SynchronousGrammar.isTerminal(token)) {
        token
      } else {
        nonTerminalIndex += 1
        val nonTerminalRulesIndex = parse.rule.nonTerminalAlignment(nonTerminalIndex)
        this.fromParse(parse.nonTerminalRules(nonTerminalRulesIndex))
      }
    }
    val targetList = Temporal.handleSpecials(targetSeq.toList)
    parse.rule.basicSymbol match {
      case "[Number]" => Temporal.Number(targetList)
      case "[Unit]" => Temporal.Unit(targetList)
      case "[Field]" => Temporal.Field(targetList)
      case "[Period]" => Temporal.Period(targetList)
      case "[Anchor]" => Temporal.Anchor(targetList)
    }
  }

  private[Temporal] def fail[T](name: String, args: List[AnyRef]): T = {
    throw new UnsupportedOperationException(
      "Don't know how to parse %s from %s".format(name, args))
  }

  private[Temporal] def handleSpecials(args: List[AnyRef]): List[AnyRef] = args match {
    case "TODAY" :: tail =>
      Temporal.Anchor.Today :: handleSpecials(tail)
    case "(" :: "Period" :: (amount: String) :: (unit: String) :: ")" :: tail =>
      Temporal.Period.SimplePeriod(amount.toInt, ChronoUnit.valueOf(unit)) :: handleSpecials(tail)
    case other :: tail =>
      other :: handleSpecials(tail)
    case Nil =>
      Nil
  }

  case class Number(value: Int) extends Temporal
  object Number {
    def apply(args: List[AnyRef]): Number = args match {
      case (number: Temporal.Number) :: Nil =>
        number
      case (number: String) :: Nil =>
        Number(number.toInt)
      case _ =>
        fail("Number", args)
    }
  }

  case class Unit(value: ChronoUnit) extends Temporal
  object Unit {
    def apply(args: List[AnyRef]): Unit = args match {
      case (unit: Unit) :: Nil =>
        unit
      case (unit: String) :: Nil =>
        Temporal.Unit(ChronoUnit.valueOf(unit))
      case _ =>
        fail("Unit", args)
    }
  }

  case class Field(name: ChronoField, value: Int) extends Temporal
  object Field {
    def apply(args: List[AnyRef]): Field = args match {
      case (field: Field) :: Nil =>
        field
      case (field: String) :: (number: String) :: Nil =>
        Field(ChronoField.valueOf(field), number.toInt)
      case (field: String) :: (number: Temporal.Number) :: Nil =>
        Field(ChronoField.valueOf(field), number.value)
      case _ =>
        fail("Field", args)
    }
  }

  sealed trait Anchor extends Temporal {
    
    def chronoFields: Set[ChronoField]
    
    def toDateTime(anchor: ZonedDateTime): ZonedDateTime

    def toTimeMLValue(anchor: ZonedDateTime): String = {
      val minDuration = this.chronoFields.map(_.getBaseUnit.getDuration).min
      val isBigEnough = (fieldFormat: (ChronoField, String)) => {
        val duration = fieldFormat._1.getBaseUnit.getDuration
        duration.equals(minDuration) || duration.isGreaterThan(minDuration)
      }
      val dateTime = this.toDateTime(anchor)
      val parts = for ((field, format) <- this.fieldFormats.takeWhile(isBigEnough)) yield {
        format.format(dateTime.get(field))
      }
      parts.mkString
    }

    private val fieldFormats = Seq(
      ChronoField.YEAR -> "%04d",
      ChronoField.MONTH_OF_YEAR -> "-%02d",
      ChronoField.DAY_OF_MONTH -> "-%02d")
  }

  object Anchor {

    def apply(args: List[AnyRef]): Anchor = args match {
      case (anchor: Anchor) :: Nil =>
        anchor
      case "Date" :: Field(ChronoField.YEAR, year) :: Field(ChronoField.MONTH_OF_YEAR, month) :: Field(ChronoField.DAY_OF_MONTH, day) :: Nil =>
        Date(year, month, day)
      case "Next" :: tail =>
        Next(tail.map { case field: Field => (field.name, field.value) }.toMap)
      case "Previous" :: tail =>
        Previous(tail.map { case field: Field => (field.name, field.value) }.toMap)
      case "Plus" :: (anchor: Anchor) :: (period: Period) :: Nil =>
        Plus(anchor, period)
      case "Minus" :: (anchor: Anchor) :: (period: Period) :: Nil =>
        Minus(anchor, period)
      case _ =>
        fail("Anchor", args)
    }

    case object Today extends Anchor {

      val chronoFields = Set(ChronoField.DAY_OF_MONTH)
      
      def toDateTime(anchor: ZonedDateTime) = anchor
    }

    case class Date(year: Int, month: Int, day: Int) extends Anchor {
      
      val chronoFields = Set(ChronoField.DAY_OF_MONTH)
      
      def toDateTime(anchor: ZonedDateTime) = {
        anchor.withYear(year).withMonth(month).withDayOfMonth(day)
      }
    }

    case class Next(fields: Map[ChronoField, Int]) extends Anchor {
      
      val chronoFields = fields.keySet
      
      def toDateTime(anchor: ZonedDateTime) = {
        anchor.plus(new FollowingAdjuster(fields))
      }
    }

    case class Previous(fields: Map[ChronoField, Int]) extends Anchor {

      val chronoFields = fields.keySet
      
      def toDateTime(anchor: ZonedDateTime) = {
        anchor.minus(new PreviousAdjuster(fields))
      }
    }

    case class Plus(anchor: Anchor, period: Period) extends Anchor {
      
      val chronoFields = anchor.chronoFields
      
      def toDateTime(anchorDateTime: ZonedDateTime) = {
        var result = anchor.toDateTime(anchorDateTime)
        for ((unit, amount) <- period.toUnitCounts) {
          result = result.plus(amount, unit)
        }
        result
      }
    }

    case class Minus(anchor: Anchor, period: Period) extends Anchor {

      val chronoFields = anchor.chronoFields
      
      def toDateTime(anchorDateTime: ZonedDateTime) = {
        var result = anchor.toDateTime(anchorDateTime)
        for ((unit, amount) <- period.toUnitCounts) {
          result = result.minus(amount, unit)
        }
        result
      }
    }

    private abstract class SearchingAdjuster(constraints: Map[ChronoField, Int]) {
      val unit = constraints.keySet.map(_.getBaseUnit).minBy(_.getDuration)
      def adjustInto(temporal: JTemporal, adjust: JTemporal => JTemporal): JTemporal = {
        var curr = adjust(temporal)
        while (constraints.exists { case (field, value) => curr.get(field) != value }) {
          curr = adjust(curr)
        }
        curr
      }
    }

    private class PreviousAdjuster(constraints: Map[ChronoField, Int]) extends SearchingAdjuster(constraints) with TemporalSubtractor {
      def subtractFrom(temporal: JTemporal): JTemporal = {
        this.adjustInto(temporal, _.minus(1, this.unit))
      }
    }

    private class FollowingAdjuster(constraints: Map[ChronoField, Int]) extends SearchingAdjuster(constraints) with TemporalAdder {
      def addTo(temporal: JTemporal): JTemporal = {
        this.adjustInto(temporal, _.plus(1, this.unit))
      }
    }
  }

  sealed trait Period extends Temporal {

    def toUnitCounts: Map[ChronoUnit, Int]

    def toTimeMLValue: String = {
      val counts = this.toUnitCounts
      val parts = for ((unit, char) <- this.unitChars) yield counts.get(unit).map(_ + char)
      "P" + parts.flatten.mkString
    }

    private val unitChars = Seq(
      ChronoUnit.YEARS -> "Y",
      ChronoUnit.MONTHS -> "M",
      ChronoUnit.WEEKS -> "W",
      ChronoUnit.DAYS -> "D")
  }

  object Period {

    def apply(args: List[AnyRef]): Period = args match {
      case (period: Period) :: Nil =>
        period
      case (unit: Unit) :: Nil =>
        SimplePeriod(1, unit.value)
      case (amount: Number) :: (unit: Unit) :: Nil =>
        SimplePeriod(amount.value, unit.value)
      case "Sum" :: (period1: Period) :: (period2: Period) :: Nil =>
        Plus(period1, period2)
      case _ =>
        fail("Period", args)
    }

    case class SimplePeriod(amount: Int, unit: ChronoUnit) extends Period {
      def toUnitCounts = Map(unit -> amount).withDefaultValue(0)
    }

    case class Plus(period1: Period, period2: Period) extends Period {
      def toUnitCounts = {
        val counts1 = period1.toUnitCounts
        val counts2 = period2.toUnitCounts
        val pairs = for (unit <- counts1.keySet ++ counts2.keySet) yield {
          (unit, counts1(unit) + counts2(unit))
        }
        pairs.toMap
      }
    }

    case class Minus(period1: Period, period2: Period) extends Period {
      def toUnitCounts = {
        val counts1 = period1.toUnitCounts
        val counts2 = period2.toUnitCounts
        val pairs = for (unit <- counts1.keySet ++ counts2.keySet) yield {
          (unit, counts1(unit) - counts2(unit))
        }
        pairs.toMap
      }
    }
  }

  sealed trait Mod extends Temporal
  object Mod {
    case object Exact extends Mod
    case object Before extends Mod
    case object After extends Mod
    case object OnOrBefore extends Mod
    case object OnOrAfter extends Mod
    case object LessThan extends Mod
    case object MoreThan extends Mod
    case object EqualOrLess extends Mod
    case object EqualOrMore extends Mod
    case object Start extends Mod
    case object Mid extends Mod
    case object End extends Mod
    case object Approx extends Mod
  }
}