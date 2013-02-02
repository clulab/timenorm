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

import info.bethard.timenorm.SynchronousParser.Tree

sealed trait Temporal

object Temporal {

  def fromParse(tree: Tree.NonTerminal): Temporal = {
    tree.rule.basicSymbol match {
      case "[Number]" => Temporal.Number.fromParse(tree)
      case "[Unit]" => Temporal.Unit.fromParse(tree)
      case "[Field]" => Temporal.Field.fromParse(tree)
      case "[Period]" => Temporal.Period.fromParse(tree)
      case "[Anchor]" => Temporal.Anchor.fromParse(tree)
    }
  }

  private[Temporal] def fail[T](name: String, tree: Tree): T = {
    throw new UnsupportedOperationException(
      "Don't know how to parse %s from %s".format(name, tree match {
        case tree: Tree.Terminal => tree.token
        case tree: Tree.NonTerminal => tree.children.map {
          case child: Tree.Terminal => "Terminal:" + child.token
          case child: Tree.NonTerminal => "NonTerminal:" + child.rule.symbol
        }
      }))
  }

  case class Number(value: Int) extends Temporal
  object Number {
    def fromParse(tree: Tree.NonTerminal): Number = tree.children match {
      case Tree.Terminal(number) :: Nil =>
        Number(number.toInt)
      case _ =>
        fail("Number", tree)
    }
  }

  case class Unit(value: ChronoUnit) extends Temporal
  object Unit {
    def fromParse(tree: Tree.NonTerminal): Unit = tree.children match {
      case Tree.Terminal(unit) :: Nil =>
        Temporal.Unit(ChronoUnit.valueOf(unit))
      case _ =>
        fail("Unit", tree)
    }
  }

  case class Field(name: ChronoField, value: Int) extends Temporal
  object Field {
    def fromParse(tree: Tree.NonTerminal): Field = tree.children match {
      case Tree.Terminal(field) :: Tree.Terminal(number) :: Nil =>
        Field(ChronoField.valueOf(field), number.toInt)
      case Tree.Terminal(field) :: (number: Tree.NonTerminal) :: Nil =>
        Field(ChronoField.valueOf(field), Number.fromParse(number).value)
      case _ =>
        fail("Field", tree)
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

    def fromParse(tree: Tree.NonTerminal): Anchor = tree.children match {
      case Tree.Terminal("TODAY") :: Nil =>
        Temporal.Anchor.Today
      case Tree.Terminal("Date") :: (year: Tree.NonTerminal) :: (month: Tree.NonTerminal) :: (day: Tree.NonTerminal) :: Nil =>
        Date(Field.fromParse(year).value, Field.fromParse(month).value, Field.fromParse(day).value)
      case Tree.Terminal("Next") :: tail =>
        Next(this.toFieldNameValuePairs(tail).toMap)
      case Tree.Terminal("Previous") :: tail =>
        Previous(this.toFieldNameValuePairs(tail).toMap)
      case Tree.Terminal("Plus") :: (anchor: Tree.NonTerminal) :: (period: Tree.NonTerminal) :: Nil =>
        Plus(Anchor.fromParse(anchor), Period.fromParse(period))
      case Tree.Terminal("Plus") :: Tree.Terminal("TODAY") :: (period: Tree.NonTerminal) :: Nil =>
        Plus(Temporal.Anchor.Today, Period.fromParse(period))
      case Tree.Terminal("Plus") :: Tree.Terminal("TODAY") :: Tree.Terminal("(") :: Tree.Terminal("Period") :: Tree.Terminal(amount) :: Tree.Terminal(unit) :: Tree.Terminal(")") :: Nil =>
        Plus(Temporal.Anchor.Today, Period.SimplePeriod(amount.toInt, ChronoUnit.valueOf(unit)))
      case Tree.Terminal("Minus") :: (anchor: Tree.NonTerminal) :: (period: Tree.NonTerminal) :: Nil =>
        Minus(Anchor.fromParse(anchor), Period.fromParse(period))
      case Tree.Terminal("Minus") :: Tree.Terminal("TODAY") :: (period: Tree.NonTerminal) :: Nil =>
        Minus(Temporal.Anchor.Today, Period.fromParse(period))
      case Tree.Terminal("Minus") :: Tree.Terminal("TODAY") :: Tree.Terminal("(") :: Tree.Terminal("Period") :: Tree.Terminal(amount) :: Tree.Terminal(unit) :: Tree.Terminal(")") :: Nil =>
        Minus(Temporal.Anchor.Today, Period.SimplePeriod(amount.toInt, ChronoUnit.valueOf(unit)))
      case _ =>
        fail("Anchor", tree)
    }

    private def toFieldNameValuePairs(trees: List[Tree]): List[(ChronoField, Int)] = {
      trees.map {
        case tree: Tree.NonTerminal =>
          val field = Field.fromParse(tree)
          (field.name, field.value)
        case tree: Tree.Terminal =>
          fail("Field", tree)
      }
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

    def fromParse(tree: Tree.NonTerminal): Period = tree.children match {
      case (unit: Tree.NonTerminal) :: Nil =>
        SimplePeriod(1, Unit.fromParse(unit).value)
      case (amount: Tree.NonTerminal) :: (unit: Tree.NonTerminal) :: Nil =>
        SimplePeriod(Number.fromParse(amount).value, Unit.fromParse(unit).value)
      case Tree.Terminal("Sum") :: (period1: Tree.NonTerminal) :: (period2: Tree.NonTerminal) :: Nil =>
        Plus(Period.fromParse(period1), Period.fromParse(period2))
      case _ =>
        fail("Period", tree)
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