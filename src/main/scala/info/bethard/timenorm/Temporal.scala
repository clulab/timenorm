package info.bethard.timenorm

import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalAdjuster
import org.threeten.bp.temporal.{Temporal => JTemporal}
import org.threeten.bp.temporal.TemporalAdder
import org.threeten.bp.temporal.TemporalSubtractor
import org.threeten.bp.temporal.ChronoField

sealed trait Temporal

object Temporal {
  
  private[Temporal] def fail[T](name: String, args: List[AnyRef]): T = {
    throw new UnsupportedOperationException(
        "Don't know how to parse %s from %s".format(name, args))
  }
  
  private[timenorm] def handleSpecials(args: List[AnyRef]): List[AnyRef] = args match {
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

  sealed trait Anchor extends Temporal
  object Anchor {
    case object Today extends Anchor
    case class Of(fields: Map[ChronoField, Int]) extends Anchor
    case class Plus(anchor: Anchor, period: Period) extends Anchor
    case class Minus(anchor: Anchor, period: Period) extends Anchor

    def apply(args: List[AnyRef]): Anchor = args match {
      case (anchor: Anchor) :: Nil =>
        anchor
      case "Plus" :: (anchor: Anchor) :: (period: Period) :: Nil =>
        Plus(anchor, period)
      case "Minus" :: (anchor: Anchor) :: (period: Period) :: Nil =>
        Minus(anchor, period)
      case fields if fields.forall(_.isInstanceOf[Field]) =>
        Of(fields.collect { case field: Field => (field.name, field.value) }.toMap)
      case _ =>
        fail("Anchor", args)
    }
  }

  sealed trait Period extends Temporal
  object Period {
    case class SimplePeriod(amount: Int, unit: ChronoUnit) extends Period
    case class Plus(period1: Period, period2: Period) extends Period
    case class Minus(period1: Period, period2: Period) extends Period
    
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