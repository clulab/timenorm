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

  def fromParse(parse: Parser.Parse): Temporal = {
    var nonTerminalIndex = -1
    val targetSeq = for ((token, i) <- parse.rule.targetSeq.zipWithIndex) yield {
      if (Grammar.isTerminal(token)) {
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
    def toTimeMLValue(anchor: ZonedDateTime): String
    protected val fieldFormats = Seq(
      ChronoField.YEAR -> "%04d",
      ChronoField.MONTH_OF_YEAR -> "-%02d",
      ChronoField.DAY_OF_MONTH -> "-%02d")
  }
  object Anchor {
    case object Today extends Anchor {
      def toTimeMLValue(anchor: ZonedDateTime) = anchor.getDate().toString
    }
    case class Date(year: Int, month: Int, day: Int) extends Anchor {
      def toTimeMLValue(anchor: ZonedDateTime): String = ???
    }
    case class Next(fields: Map[ChronoField, Int]) extends Anchor {
      def toTimeMLValue(anchor: ZonedDateTime): String = ???
    }
    case class Previous(fields: Map[ChronoField, Int]) extends Anchor {
      def toTimeMLValue(anchor: ZonedDateTime): String = ???
    }
    case class Plus(anchor: Anchor, period: Period) extends Anchor {
      def toTimeMLValue(anchor: ZonedDateTime): String = ???
    }
    case class Minus(anchor: Anchor, period: Period) extends Anchor {
      def toTimeMLValue(anchor: ZonedDateTime): String = ???
    }

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

  // TODO: the below are unused at the moment
  abstract class SearchingAdjuster(constraints: Seq[(TemporalField, Int)]) extends TemporalAdjuster {
    val unit = constraints.map(_._1.getBaseUnit).minBy(_.getDuration)
    def adjustInto(temporal: JTemporal): JTemporal = {
      var curr = this.adjust(temporal)
      while (constraints.exists { case (field, value) => curr.get(field) != value }) {
        curr = this.adjust(curr)
      }
      curr
    }
    def adjust(dateTime: JTemporal): JTemporal
  }

  class PreviousAdjuster(constraints: Seq[(TemporalField, Int)]) extends SearchingAdjuster(constraints) with TemporalAdder {
    def addTo(temporal: JTemporal): JTemporal = this.adjustInto(temporal)
    def adjust(temporal: JTemporal) = temporal.minus(1, this.unit)
  }

  class FollowingAdjuster(constraints: Seq[(TemporalField, Int)]) extends SearchingAdjuster(constraints) with TemporalSubtractor {
    def subtractFrom(temporal: JTemporal): JTemporal = this.adjustInto(temporal)
    def adjust(temporal: JTemporal) = temporal.plus(1, this.unit)
  }
}