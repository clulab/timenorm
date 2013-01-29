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
  case class Number(value: Int) extends Temporal
  case class Unit(value: ChronoUnit) extends Temporal
  case class Field(name: ChronoField, value: Int) extends Temporal

  sealed trait Anchor extends Temporal
  object Anchor {
    case object Today extends Anchor
    case class OfFields(fields: Map[ChronoField, Int]) extends Anchor
    case class Plus(anchor: Anchor, period: Period) extends Anchor
    case class Minus(anchor: Anchor, period: Period) extends Anchor
  }

  sealed trait Period extends Temporal
  object Period {
    case class SimplePeriod(amount: Int, unit: ChronoUnit) extends Period
    case class Plus(period1: Period, period2: Period) extends Period
    case class Minus(period1: Period, period2: Period) extends Period
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