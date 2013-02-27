package info.bethard.timenorm
import scala.collection.immutable.Seq

import org.threeten.bp.ZonedDateTime
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.{ Temporal => JTemporal }
import org.threeten.bp.temporal.TemporalAdder
import org.threeten.bp.temporal.TemporalSubtractor
import org.threeten.bp.temporal.TemporalUnit

import info.bethard.timenorm.SynchronousParser.Tree

class TemporalParser(grammar: SynchronousGrammar) {

  val parser = new SynchronousParser(grammar)

  def parseAll(sourceTokens: Seq[String]): Seq[TemporalParse] = {
    this.parser.parseAll(sourceTokens).map(TemporalParse.apply)
  }

  def parseAll(sourceTokens: Array[String]): Array[TemporalParse] = {
    this.parseAll(sourceTokens.toIndexedSeq).toArray
  }

  def parseAll(sourceText: String): Array[TemporalParse] = {
    this.parseAll(sourceText.split("\\s*\\b\\s*").filter(!_.matches("\\s*")))
  }
}

private[timenorm] abstract class CanFail(name: String) {
  private[timenorm] def fail[T](tree: Tree): T = {
    throw new UnsupportedOperationException(
      "Don't know how to parse %s from %s".format(this.name, tree match {
        case tree: Tree.Terminal => tree.token
        case tree: Tree.NonTerminal => tree.rule.basicSymbol + " -> " + tree.children.map {
          case child: Tree.Terminal => child.token
          case child: Tree.NonTerminal => child.rule.symbol
        }.mkString(" ")
      }))
  }
}

sealed abstract class TemporalParse

object TemporalParse extends CanFail("[Temporal]") with (Tree => TemporalParse) {

  def apply(tree: Tree): TemporalParse = tree match {
    case tree @ Tree.NonTerminal("[Period]", _, _, _) =>
      PeriodParse(tree)
    case tree @ Tree.NonTerminal("[Time]", _, _, _) =>
      TimeParse(tree)
    case _ => fail(tree)
  }
}

case class NumberParse(value: Int)
object NumberParse extends CanFail("[Number]") with (Tree => NumberParse) {
  def apply(tree: Tree): NumberParse = tree match {
    case Tree.Terminal(number) =>
      NumberParse(number.toInt)
    case Tree.NonTerminal("[Number]", _, tree :: Nil, _) =>
      NumberParse(tree)
    case _ => fail(tree)
  }
}

case class UnitParse(value: ChronoUnit)
object UnitParse extends CanFail("[Unit]") with (Tree => UnitParse) {
  def apply(tree: Tree): UnitParse = tree match {
    case Tree.Terminal(unit) =>
      UnitParse(ChronoUnit.valueOf(unit))
    case Tree.NonTerminal("[Unit]", _, tree :: Nil, _) =>
      UnitParse(tree)
    case _ => fail(tree)
  }
}

case class FieldValueParse(name: ChronoField, value: Int)
object FieldValueParse extends CanFail("[FieldValue]") with (Tree => FieldValueParse) {
  def apply(tree: Tree): FieldValueParse = tree match {
    case Tree.NonTerminal("[FieldValue]", _, tree :: Nil, _) =>
      FieldValueParse(tree)
    case Tree.NonTerminal("[FieldValue]", _, Tree.Terminal(field) :: number :: Nil, _) =>
      FieldValueParse(ChronoField.valueOf(field), NumberParse(number).value)
    case _ => fail(tree)
  }
}

sealed abstract class PeriodParse extends TemporalParse {
  def toPeriod: Period
}

object PeriodParse extends CanFail("[Period]") with (Tree => PeriodParse) {

  def apply(tree: Tree): PeriodParse = tree match {
    case tree: Tree.Terminal =>
      Simple(1, UnitParse(tree).value)
    case Tree.NonTerminal(_, "[Period]", tree :: Nil, _) =>
      PeriodParse(tree)
    case Tree.NonTerminal(_, "[Period:Simple]", unit :: Nil, _) =>
      Simple(1, UnitParse(unit).value)
    case Tree.NonTerminal(_, "[Period:Simple]", amount :: unit :: Nil, _) =>
      Simple(NumberParse(amount).value, UnitParse(unit).value)
    case Tree.NonTerminal(_, "[Period:Sum]", children, _) =>
      Sum(children.map(PeriodParse))
    case Tree.NonTerminal(_, "[Period:Modifier]", Tree.Terminal(modifier) :: period :: Nil, _) =>
      Modifier(modifier, PeriodParse(period))
    case _ => fail(tree)
  }

  case class Simple(amount: Int, unit: ChronoUnit) extends PeriodParse {
    def toPeriod = Period(Map(unit -> amount))
  }

  case class Sum(periodParses: Seq[PeriodParse]) extends PeriodParse {
    def toPeriod = periodParses.foldLeft(Period(Map.empty))(_ + _.toPeriod)
  }

  case class Modifier(modifier: String, periodParse: PeriodParse) extends PeriodParse {
    def toPeriod = periodParse.toPeriod.copy(modifier = modifier)
  }
}

sealed abstract class TimeParse extends TemporalParse {
  def toDateTime(anchor: ZonedDateTime): DateTime
}

object TimeParse extends CanFail("[Time]") with (Tree => TimeParse) {

  def apply(tree: Tree): TimeParse = tree match {
    case Tree.Terminal("PAST") =>
      Past
    case Tree.Terminal("PRESENT") =>
      Present
    case Tree.Terminal("FUTURE") =>
      Future
    case Tree.NonTerminal(_, "[Time]", tree :: Nil, _) =>
      TimeParse(tree)
    case Tree.NonTerminal(_, "[Time:Simple]", (tree: Tree.Terminal) :: Nil, _) =>
      TimeParse(tree)
    case Tree.NonTerminal(_, "[Time:Absolute]", children, _) =>
      Absolute(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[Time:Next]", children, _) =>
      Next(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[Time:Previous]", children, _) =>
      Previous(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[Time:CurrentOrPrevious]", children, _) =>
      CurrentOrPrevious(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[Time:Closest]", children, _) =>
      Closest(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[Time:Plus]", time :: period :: Nil, _) =>
      Plus(TimeParse(time), PeriodParse(period))
    case Tree.NonTerminal(_, "[Time:Minus]", time :: period :: Nil, _) =>
      Minus(TimeParse(time), PeriodParse(period))
    case Tree.NonTerminal(_, "[Time:WithUnit]", time :: unit :: Nil, _) =>
      WithUnit(TimeParse(time), UnitParse(unit).value)
    case Tree.NonTerminal(_, "[Time:Modifier]", Tree.Terminal(modifier) :: time :: Nil, _) =>
      Modifier(modifier, TimeParse(time))
    case _ => fail(tree)
  }

  private def toFieldValues(trees: List[Tree]): Map[ChronoField, Int] = {
    trees.map(FieldValueParse).map(field => (field.name, field.value)).toMap
  }

  case object Past extends TimeParse {
    def toDateTime(anchor: ZonedDateTime) = {
      new DateTime(anchor, ChronoUnit.FOREVER, ChronoUnit.FOREVER) {
        override def toTimeMLValue(unit: TemporalUnit) = "PAST_REF"
      }
    }
  }

  case object Present extends TimeParse {
    def toDateTime(anchor: ZonedDateTime) = {
      new DateTime(anchor, ChronoUnit.SECONDS, ChronoUnit.FOREVER) {
        override def toTimeMLValue(unit: TemporalUnit) = "PRESENT_REF"
      }
    }
  }

  case object Future extends TimeParse {
    def toDateTime(anchor: ZonedDateTime) = {
      new DateTime(anchor, ChronoUnit.FOREVER, ChronoUnit.FOREVER) {
        override def toTimeMLValue(unit: TemporalUnit) = "FUTURE_REF"
      }
    }
  }

  abstract class FieldBasedTimeParse(fields: Map[ChronoField, Int]) extends TimeParse {
    val minUnit = fields.keySet.map(_.getBaseUnit).minBy(_.getDuration)
  }

  case class Absolute(fields: Map[ChronoField, Int]) extends FieldBasedTimeParse(fields) {
    def toDateTime(anchor: ZonedDateTime) = {
      var curr = anchor
      for ((field, value) <- fields) {
        curr = curr.`with`(field, value)
      }
      DateTime(curr, this.minUnit, this.minUnit)
    }
  }

  abstract class FieldSearchingTimeParse(fields: Map[ChronoField, Int]) extends FieldBasedTimeParse(fields) {
    def searchFrom(dateTime: ZonedDateTime, step: (ZonedDateTime, TemporalUnit) => ZonedDateTime): ZonedDateTime = {
      var curr = dateTime
      while (fields.exists { case (field, value) => curr.get(field) != value }) {
        curr = step(curr, this.minUnit)
      }
      curr
    }
  }

  abstract class DirectedFieldSearchingTimeParse(
    fields: Map[ChronoField, Int],
    stepFirst: (ZonedDateTime, TemporalUnit) => ZonedDateTime,
    step: (ZonedDateTime, TemporalUnit) => ZonedDateTime)
      extends FieldSearchingTimeParse(fields) {

    def toDateTime(dateTime: ZonedDateTime) = {
      val adjustedDateTime = this.searchFrom(this.stepFirst(dateTime, this.minUnit), this.step)
      DateTime(adjustedDateTime, this.minUnit, this.minUnit)
    }
  }

  case class Next(fields: Map[ChronoField, Int])
    extends DirectedFieldSearchingTimeParse(fields, _.plus(1, _), _.plus(1, _))

  case class Previous(fields: Map[ChronoField, Int])
    extends DirectedFieldSearchingTimeParse(fields, _.minus(1, _), _.minus(1, _))

  case class CurrentOrPrevious(fields: Map[ChronoField, Int])
    extends DirectedFieldSearchingTimeParse(fields, _.minus(0, _), _.minus(1, _))

  case class Closest(fields: Map[ChronoField, Int]) extends FieldSearchingTimeParse(fields) {
    def toDateTime(dateTime: ZonedDateTime) = {
      val prev = this.searchFrom(dateTime, _.minus(1, _))
      val next = this.searchFrom(dateTime, _.plus(1, _))
      val distToPrev = prev.periodUntil(dateTime, this.minUnit)
      val distToNext = dateTime.periodUntil(next, this.minUnit)
      val adjustedDateTime = if (distToPrev < distToNext) prev else next
      DateTime(adjustedDateTime, this.minUnit, this.minUnit)
    }
  }

  abstract class PeriodTimeParse(
      anchorParse: TimeParse,
      periodParse: PeriodParse,
      adjust: (ZonedDateTime, Int, TemporalUnit) => ZonedDateTime) extends TimeParse {

    def toDateTime(anchorDateTime: ZonedDateTime) = {
      val dateTime = anchorParse.toDateTime(anchorDateTime)
      var fullDateTime = dateTime.fullDateTime
      val unitAmounts = periodParse.toPeriod.unitAmounts
      for ((unit, amount) <- unitAmounts) {
        fullDateTime = adjust(fullDateTime, amount, unit)
      }
      val minUnit = unitAmounts.keySet.minBy(_.getDuration)
      DateTime(fullDateTime, dateTime.baseUnit, minUnit)
    }

  }

  case class Plus(anchor: TimeParse, period: PeriodParse)
    extends PeriodTimeParse(anchor, period, _.plus(_, _))

  case class Minus(anchor: TimeParse, period: PeriodParse)
    extends PeriodTimeParse(anchor, period, _.minus(_, _))

  case class WithUnit(anchorParse: TimeParse, unit: ChronoUnit) extends TimeParse {
    def toDateTime(anchor: ZonedDateTime) = {
      anchorParse.toDateTime(anchor).copy(baseUnit = unit, rangeUnit = unit)
    }
  }

  case class Modifier(modifier: String, anchor: TimeParse) extends TimeParse {
    def toDateTime(zonedDateTime: ZonedDateTime) = {
      val dateTime = anchor.toDateTime(zonedDateTime)
      new DateTime(dateTime.fullDateTime, dateTime.baseUnit, dateTime.rangeUnit, modifier)
    }
  }
}
