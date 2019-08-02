package org.clulab.timenorm.scfg

import java.time.temporal._
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}

import org.clulab.time._
import org.clulab.timenorm.scfg.PeriodSet.{Frequency, Quantifier}
import org.clulab.timenorm.scfg.SynchronousParser.Tree

import scala.collection.immutable.Seq


trait TokenParser {
  def toInt(token: String): Int
  def toTemporalUnit(token: String): TemporalUnit
  def toTemporalField(token: String): TemporalField
}

class DefaultTokenParser extends TokenParser {
  def toInt(token: String): Int = token.toInt
  def toTemporalUnit(token: String): TemporalUnit = token match {
    case "MORNINGS" => MORNINGS
    case "AFTERNOONS" => AFTERNOONS
    case "EVENINGS" => EVENINGS
    case "NIGHTS" => NIGHTS
    case "WEEKENDS" => WEEKENDS
    case "SPRINGS" => SPRINGS
    case "SUMMERS" => SUMMERS
    case "FALLS" => FALLS
    case "WINTERS" => WINTERS
    case "QUARTER_YEARS" => IsoFields.QUARTER_YEARS
    case "WEEK_BASED_YEARS" => IsoFields.WEEK_BASED_YEARS
    case "UNSPECIFIED" => UNSPECIFIED
    case _ => ChronoUnit.valueOf(token)
  }
  def toTemporalField(token: String): TemporalField = token match {
    case "MORNING_OF_DAY" => MORNING_OF_DAY
    case "AFTERNOON_OF_DAY" => AFTERNOON_OF_DAY
    case "EVENING_OF_DAY" => EVENING_OF_DAY
    case "NIGHT_OF_DAY" => NIGHT_OF_DAY
    case "EASTER_DAY_OF_YEAR" => EASTER_DAY_OF_YEAR
    case "DAY_OF_WEEKEND" => DAY_OF_WEEKEND
    case "WEEKEND_OF_WEEK" => WEEKEND_OF_WEEK
    case "DAY_OF_SPRING" => DAY_OF_SPRING
    case "SPRING_OF_YEAR" => SPRING_OF_YEAR
    case "DAY_OF_SUMMER" => DAY_OF_SUMMER
    case "SUMMER_OF_YEAR" => SUMMER_OF_YEAR
    case "DAY_OF_FALL" => DAY_OF_FALL
    case "FALL_OF_YEAR" => FALL_OF_YEAR
    case "DAY_OF_WINTER" => DAY_OF_WINTER
    case "WINTER_OF_YEAR" => WINTER_OF_YEAR
    case "YEAR_OF_DECADE" => YEAR_OF_DECADE
    case "DECADE" => DECADE
    case "DECADE_OF_CENTURY" => DECADE_OF_CENTURY
    case "YEAR_OF_CENTURY" => YEAR_OF_CENTURY
    case "CENTURY" => CENTURY
    // the pattern matcher of 2.10.0 can't handle such big case statements, so break it up a bit
    case token => token match {
      case "DAY_OF_QUARTER" => IsoFields.DAY_OF_QUARTER
      case "QUARTER_OF_YEAR" => IsoFields.QUARTER_OF_YEAR
      case "WEEK_BASED_YEAR" => IsoFields.WEEK_BASED_YEAR
      case "WEEK_OF_WEEK_BASED_YEAR" => IsoFields.WEEK_OF_WEEK_BASED_YEAR
      case _ => ChronoField.valueOf(token)
    }
  }
}
object DefaultTokenParser extends DefaultTokenParser

private[timenorm] abstract class CanFail(name: String) {
  private[timenorm] def fail[T](tree: Tree): T = {
    throw new UnsupportedOperationException(
      "Don't know how to parse %s from %s".format(this.name, tree match {
        case tree: Tree.Terminal => tree.token
        case tree: Tree.NonTerminal => tree.rule.symbol + " -> " + tree.children.map {
          case child: Tree.Terminal => child.token
          case child: Tree.NonTerminal => child.rule.symbol
        }.mkString(" ")
      }))
  }
}

sealed abstract class TemporalParse

object TemporalParse extends CanFail("[Temporal]") with (Tree => TemporalParse) {
  
  def apply(tree: Tree): TemporalParse = {
    this.applyNoImplicit(tree, DefaultTokenParser)
  }
  
  def apply(tree: Tree)(implicit tokenParser: TokenParser): TemporalParse = {
    this.applyNoImplicit(tree, tokenParser)
  }

  private def applyNoImplicit(tree: Tree, tokenParser: TokenParser): TemporalParse = {
    implicit val parser = tokenParser
    tree match {
      case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
        case "[Period]" => PeriodParse(tree)
        case "[PeriodSet]" => PeriodSetParse(tree)
        case "[TimeSpan]" => TimeSpanParse(tree)
        case "[TimeSpanSet]" => TimeSpanSetParse(tree)
        case _ => fail(tree)
      }
      case _ => fail(tree)
    }
  }
}

case class IntParse(value: Int)
object IntParse extends CanFail("[Int]") {
  def apply(tree: Tree)(implicit tokenParser: TokenParser): IntParse = tree match {
    case Tree.Terminal(number) =>
      IntParse(tokenParser.toInt(number))
    case tree =>
      val number = this.toDigits(tree).reverse.zipWithIndex.foldLeft(0){
        case (sum, (digit, index)) => sum + digit * math.pow(10, index).toInt
      }
      IntParse(number)
  }
  
  private def toDigits(tree: Tree): List[Int] = tree match {
    case Tree.Terminal(number) =>
      number.toInt :: Nil
    case Tree.NonTerminal(rule, children) if rule.basicSymbol == "[Int]" =>
      children.flatMap(this.toDigits)
    case _ => fail(tree)
  }
}

case class UnitParse(value: TemporalUnit)
object UnitParse extends CanFail("[Unit]") {
  def apply(tree: Tree)(implicit tokenParser: TokenParser): UnitParse = tree match {
    case Tree.Terminal(unit) =>
      UnitParse(tokenParser.toTemporalUnit(unit))
    case Tree.NonTerminal(rule, tree :: Nil) if rule.basicSymbol == "[Unit]" =>
      UnitParse(tree)
    case _ => fail(tree)
  }
}

case class FieldValueParse(fieldValues: Map[TemporalField, Int]) {
  for ((field, value) <- fieldValues; if !field.range().isValidValue(value))
    throw new UnsupportedOperationException("field %s cannot have value %s".format(field, value))
}
object FieldValueParse extends CanFail("[FieldValue]") {
  def apply(tree: Tree)(implicit tokenParser: TokenParser): FieldValueParse = tree match {
    case tree: Tree.NonTerminal if tree.rule.basicSymbol == "[FieldValue]" => tree.children match {
      case Tree.Terminal(field) :: number :: Nil =>
        FieldValueParse(Map(tokenParser.toTemporalField(field) -> IntParse(number).value))
      case children =>
        FieldValueParse(children.map(FieldValueParse.apply).map(_.fieldValues).flatten.toMap)
    }
    case _ => fail(tree)
  }
}

sealed abstract class PeriodParse extends TemporalParse {
  def toPeriod: Period
}

object PeriodParse extends CanFail("[Period]") {

  def apply(tree: Tree)(implicit tokenParser: TokenParser): PeriodParse = tree match {
    case tree: Tree.Terminal =>
      Simple(1, UnitParse(tree).value)
    case tree: Tree.NonTerminal if tree.rule.basicSymbol == "[Period]" => tree.children match {
      case tree :: Nil =>
        PeriodParse(tree)
      case Tree.Terminal("Simple") :: unit :: Nil =>
        Simple(1, UnitParse(unit).value)
      case Tree.Terminal("Simple") :: amount :: unit :: Nil =>
        Simple(IntParse(amount).value, UnitParse(unit).value)
      case Tree.Terminal("Unspecified") :: unit :: Nil =>
        Unspecified(UnitParse(unit).value)
      case Tree.Terminal("Fractional") :: numerator :: denominator :: unit :: Nil =>
        Fractional(IntParse(numerator).value, IntParse(denominator).value, UnitParse(unit).value)
      case Tree.Terminal("Fractional") :: whole :: numerator :: denominator :: unit :: Nil =>
        val denominatorValue = IntParse(denominator).value
        Fractional(IntParse(whole).value * denominatorValue + IntParse(numerator).value, denominatorValue, UnitParse(unit).value)
      case Tree.Terminal("Sum") :: children =>
        Sum(children.map(PeriodParse.apply))
      case Tree.Terminal("WithModifier") :: period :: Tree.Terminal(modifier) :: Nil =>
        WithModifier(PeriodParse(period), Modifier.valueOf(modifier))
      case _ => fail(tree)
    }
    case _ => fail(tree)
  }

  case class Simple(amount: Int, unit: TemporalUnit) extends PeriodParse {
    def toPeriod = Period(Map(unit -> amount), Modifier.Exact)
  }
  
  case class Unspecified(unit: TemporalUnit) extends PeriodParse {
    def toPeriod = Period(Map(unit -> Int.MaxValue), Modifier.Exact)
  }
  
  case class Fractional(numerator: Int, denominator: Int, unit: TemporalUnit) extends PeriodParse {
    def toPeriod = Period.fromFractional(numerator, denominator, unit, Modifier.Exact)
  }

  case class Sum(periods: Seq[PeriodParse]) extends PeriodParse {
    def toPeriod = periods.foldLeft(Period.empty)(_ + _.toPeriod)
  }

  case class WithModifier(period: PeriodParse, modifier: Modifier) extends PeriodParse {
    def toPeriod = period.toPeriod.copy(modifier = modifier)
  }
}

sealed abstract class PeriodSetParse extends TemporalParse {
  def toPeriodSet: PeriodSet
}

object PeriodSetParse extends CanFail("[PeriodSet]") {

  def apply(tree: Tree)(implicit tokenParser: TokenParser): PeriodSetParse = tree match {
    case period: Tree.NonTerminal if period.rule.basicSymbol == "[Period]" =>
      Simple(PeriodParse(period))
    case tree: Tree.NonTerminal if tree.rule.basicSymbol == "[PeriodSet]" => tree.children match {
      case tree :: Nil =>
        PeriodSetParse(tree)
      case Tree.Terminal("Simple") :: period :: Nil =>
        Simple(PeriodParse(period))
      case Tree.Terminal("WithModifier") :: period :: Tree.Terminal(modifier) :: Nil =>
        WithModifier(PeriodSetParse(period), Modifier.valueOf(modifier))
      case Tree.Terminal("WithQuantifier") :: period :: Tree.Terminal(quantifier) :: Nil =>
        WithQuantifier(PeriodSetParse(period), Quantifier.valueOf(quantifier))
      case Tree.Terminal("WithFrequency") :: period :: times :: Nil =>
        WithFrequency(PeriodSetParse(period), Frequency(IntParse(times).value))
      case Tree.Terminal("WithFrequency") :: period :: times :: unit :: Nil =>
        WithFrequency(PeriodSetParse(period), Frequency(IntParse(times).value, Some(UnitParse(unit).value)))
      case _ => fail(tree)
    }
    case _ => fail(tree)
  }

  case class Simple(periodParse: PeriodParse) extends PeriodSetParse {
    def toPeriodSet = PeriodSet(periodParse.toPeriod, Modifier.Exact)
  }
  
  case class WithModifier(periodSet: PeriodSetParse, modifier: Modifier) extends PeriodSetParse {
    def toPeriodSet = periodSet.toPeriodSet.copy(modifier = modifier)
  }
  
  case class WithQuantifier(periodSet: PeriodSetParse, quantifier: Quantifier) extends PeriodSetParse {
    def toPeriodSet = periodSet.toPeriodSet.copy(quantifier = quantifier)
  }
  
  case class WithFrequency(periodSet: PeriodSetParse, frequency: Frequency) extends PeriodSetParse {
    def toPeriodSet = periodSet.toPeriodSet.copy(frequency = frequency)
  }
}

sealed abstract class TimeSpanParse extends TemporalParse {
  def toTimeSpan(anchor: TimeSpan): TimeSpan
}

object TimeSpanParse extends CanFail("[TimeSpan]") {

  def apply(tree: Tree)(implicit tokenParser: TokenParser): TimeSpanParse = tree match {
    case Tree.Terminal("PAST") =>
      Past
    case Tree.Terminal("PRESENT") =>
      Present
    case Tree.Terminal("FUTURE") =>
      Future
    case tree: Tree.NonTerminal if tree.rule.basicSymbol == "[TimeSpan]" => tree.children match {
      case tree :: Nil =>
        TimeSpanParse(tree)
      case Tree.Terminal("Simple") :: (tree: Tree.Terminal) :: Nil =>
        TimeSpanParse(tree)
      case Tree.Terminal("FindAbsolute") :: tree :: Nil =>
        FindAbsolute(FieldValueParse(tree).fieldValues)
      case Tree.Terminal("FindEarlier") :: time :: fields :: Nil =>
        FindEarlier(TimeSpanParse(time), FieldValueParse(fields).fieldValues)
      case Tree.Terminal("FindAtOrEarlier") :: time :: fields :: Nil =>
        FindAtOrEarlier(TimeSpanParse(time), FieldValueParse(fields).fieldValues)
      case Tree.Terminal("FindLater") :: time :: fields :: Nil =>
        FindLater(TimeSpanParse(time), FieldValueParse(fields).fieldValues)
      case Tree.Terminal("FindAtOrLater") :: time :: fields :: Nil =>
        FindAtOrLater(TimeSpanParse(time), FieldValueParse(fields).fieldValues)
      case Tree.Terminal("FindEnclosing") :: time :: (periodTree: Tree.NonTerminal) :: Nil if periodTree.rule.basicSymbol == "[Period]" =>
        val unit = PeriodParse(periodTree).toPeriod.unitAmounts.keySet.maxBy(_.getDuration())
        FindEnclosing(TimeSpanParse(time), unit)
      case Tree.Terminal("FindEnclosing") :: time :: (fieldTree: Tree.NonTerminal) :: Nil if fieldTree.rule.basicSymbol == "[FieldValue]" =>
        val unit = FieldValueParse(fieldTree).fieldValues.keySet.map(_.getRangeUnit()).maxBy(_.getDuration())
        FindEnclosing(TimeSpanParse(time), unit)
      case Tree.Terminal("FindEnclosing") :: time :: unit :: Nil =>
        FindEnclosing(TimeSpanParse(time), UnitParse(unit).value)
      case Tree.Terminal("FindEnclosed") :: time :: fields :: Nil =>
        FindEnclosed(TimeSpanParse(time), FieldValueParse(fields).fieldValues)
      case Tree.Terminal("StartAtStartOf") :: time :: period :: Nil =>
        StartAtStartOf(TimeSpanParse(time), PeriodParse(period))
      case Tree.Terminal("StartAtEndOf") :: time :: period :: Nil =>
        StartAtEndOf(TimeSpanParse(time), PeriodParse(period))
      case Tree.Terminal("EndAtStartOf") :: time :: period :: Nil =>
        EndAtStartOf(TimeSpanParse(time), PeriodParse(period))
      case Tree.Terminal("MoveEarlier") :: time :: period :: Nil =>
        MoveEarlier(TimeSpanParse(time), PeriodParse(period))
      case Tree.Terminal("MoveLater") :: time :: period :: Nil =>
        MoveLater(TimeSpanParse(time), PeriodParse(period))
      case Tree.Terminal("WithModifier") :: time :: Tree.Terminal(modifier) :: Nil =>
        WithModifier(TimeSpanParse(time), Modifier.valueOf(modifier))
      case _ => fail(tree)
    }
    case _ => fail(tree)
  }
  
  case object Past extends TimeSpanParse {
    def toTimeSpan(anchor: TimeSpan) = {
      new TimeSpan(TimeSpan.unspecifiedStart, anchor.start, Period.unspecified, Modifier.Approx) {
        override def timeMLValueOption = Some("PAST_REF")
      }
    }
  }

  case object Present extends TimeSpanParse {
    def toTimeSpan(anchor: TimeSpan) = {
      new TimeSpan(anchor.start, anchor.end, anchor.period, anchor.modifier) {
        override def timeMLValueOption = Some("PRESENT_REF")
      }
    }
  }

  case object Future extends TimeSpanParse {
    def toTimeSpan(anchor: TimeSpan) = {
      new TimeSpan(anchor.end, TimeSpan.unspecifiedEnd, Period.unspecified, Modifier.Approx) {
        override def timeMLValueOption = Some("FUTURE_REF")
      }
    }
  }

  abstract class FieldBasedTimeSpanParse(fields: Map[TemporalField, Int]) extends TimeSpanParse {
    val minUnit = fields.keySet.map(_.getBaseUnit).minBy(_.getDuration)
  }

  case class FindAbsolute(fields: Map[TemporalField, Int]) extends FieldBasedTimeSpanParse(fields) {
    val fieldsLargeToSmall = fields.toSeq.sortBy{
      case (field, _) => field.getRangeUnit.getDuration
    }.reverse

    def toTimeSpan: TimeSpan = {
      val zero = ZonedDateTime.of(LocalDateTime.of(1, 1, 1, 0, 0), ZoneId.of("Z"))
      val begin = this.fieldsLargeToSmall.foldLeft(zero) {
        case (time, (field, value)) => time.`with`(field, value)
      }
      val period = Period(Map(this.minUnit -> 1), Modifier.Exact)
      TimeSpan.startingAt(TimeSpan.truncate(begin, this.minUnit), period, Modifier.Exact)
    }
    def toTimeSpan(anchor: TimeSpan) = toTimeSpan
  }

  abstract class DirectedFieldSearchingTimeSpanParse(
    timeSpanParse: TimeSpanParse,
    fields: Map[TemporalField, Int],
    getStart: TimeSpan => ZonedDateTime,
    step: (ZonedDateTime, TemporalUnit) => ZonedDateTime,
    isAcceptable: (TimeSpan, TimeSpan) => Boolean) extends FieldBasedTimeSpanParse(fields) {

    val searchField = this.fields.keySet.minBy(_.getBaseUnit.getDuration)
    val period = Period(Map(this.minUnit -> 1), Modifier.Exact)

    def toTimeSpan(anchor: TimeSpan) = {
      val timeSpan = this.timeSpanParse.toTimeSpan(anchor)

      // search by base units for partial ranges (e.g. search by hours, not "mornings")
      var searchUnit = this.searchField.getBaseUnit match {
        case partialRange: PartialRange => partialRange.field.getBaseUnit
        case unit => unit
      }

      // if the field's range is fixed and the range unit is not estimated,
      // then we can move by range units once we satisfy the base unit
      var canSwitchToRange = this.searchField.range().isFixed() &&
      !this.searchField.getRangeUnit().isDurationEstimated()

      // one step at a time, search for a time that satisfies the field requirements
      var start = getStart(timeSpan)
      var result = this.tryToCreateTimeSpan(timeSpan, start)
      while (result.isEmpty) {
        start = step(start, searchUnit)

        // if we've satisfied the search field's base unit, start moving by range units
        if (canSwitchToRange && start.get(searchField) == this.fields(searchField)) {
          searchUnit = searchField.getRangeUnit()
          canSwitchToRange = false
        }
        
        // check if the current time satisfies the requirements
        result = this.tryToCreateTimeSpan(timeSpan, start)
      }
      
      // result must be present because the while loop exited
      result.get
    }
    
    private def tryToCreateTimeSpan(oldTimeSpan: TimeSpan, start: ZonedDateTime): Option[TimeSpan] = {
      // must match all field values
      if (this.fields.exists { case (field, value) => start.get(field) != value }) {
        None
      }
      // must still match all field values after truncation
      else {
        val truncatedStart = TimeSpan.truncate(start, this.minUnit)
        if (!fields.forall { case (field, value) => truncatedStart.get(field) == value }) {
          None
        }
        // must satisfy any acceptance criteria
        else {
          val timeSpan = TimeSpan.startingAt(truncatedStart, this.period, Modifier.Exact)
          if (!this.isAcceptable(oldTimeSpan, timeSpan)) None else Some(timeSpan)
        }
      }
    }
  }
  
  case class FindEarlier(timeSpanParse: TimeSpanParse, fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(
        timeSpanParse, fields, _.end, _.minus(1, _), (oldSpan, newSpan) =>
          newSpan.start.isBefore(oldSpan.start) && newSpan.end.isBefore(oldSpan.end))

  case class FindAtOrEarlier(timeSpanParse: TimeSpanParse, fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(
        timeSpanParse, fields, _.end, _.minus(1, _), (oldSpan, newSpan) =>
          !newSpan.start.isAfter(oldSpan.start) || !newSpan.end.isAfter(oldSpan.end))

  case class FindLater(timeSpanParse: TimeSpanParse, fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(
        timeSpanParse, fields, _.start, _.plus(1, _), (oldSpan, newSpan) =>
          newSpan.start.isAfter(oldSpan.start) && newSpan.end.isAfter(oldSpan.end))

  case class FindAtOrLater(timeSpanParse: TimeSpanParse, fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(
        timeSpanParse, fields, _.start, _.plus(1, _), (oldSpan, newSpan) =>
          !newSpan.start.isBefore(oldSpan.start) || !newSpan.end.isBefore(oldSpan.end))

  case class FindEnclosed(timeSpanParse: TimeSpanParse, fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(
      timeSpanParse, fields, _.start, _.plus(1, _), (oldSpan, newSpan) => {
        if (!newSpan.start.isBefore(oldSpan.end)) {
          val message = "%s not found within %s".format(fields, timeSpanParse)
          throw new UnsupportedOperationException(message)
        }
        !newSpan.start.isBefore(oldSpan.start)
      })

  case class FindEnclosing(timeSpanParse: TimeSpanParse, unit: TemporalUnit) extends TimeSpanParse {
    def toTimeSpan(anchor: TimeSpan) = {
      val timeSpan = timeSpanParse.toTimeSpan(anchor)
      if (timeSpan.period > unit) {
        throw new UnsupportedOperationException("%s is larger than 1 %s".format(timeSpanParse, unit))
      }
      var start = TimeSpan.truncate(timeSpan.start, unit)
      if (start.isAfter(timeSpan.start)) {
        start = start.minus(1, unit)
      }
      val period = Period(Map(unit -> 1), Modifier.Exact)
      TimeSpan.startingAt(start, period, timeSpan.modifier & period.modifier)
    }
  }

  abstract class MoveSpanParse(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
    extends TimeSpanParse {
    def toUnspecifiedTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier): TimeSpan
    def toTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier): TimeSpan
    def toTimeSpan(anchor: TimeSpan) = {
      val timeSpan = timeSpanParse.toTimeSpan(anchor)
      val period = periodParse.toPeriod
      val modifier = timeSpan.modifier & period.modifier
      val isUnspecified = period.unitAmounts.values.exists(_ == Int.MaxValue)
      if (isUnspecified) {
        this.toUnspecifiedTimeSpan(timeSpan, period, modifier & Modifier.Approx)
      } else {
        this.toTimeSpan(timeSpan, period, modifier)
      }
    }
  }

  case class StartAtStartOf(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends MoveSpanParse(timeSpanParse, periodParse) {
    def toUnspecifiedTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      TimeSpan(timeSpan.start, TimeSpan.unspecifiedEnd, period, modifier)
    }
    def toTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      TimeSpan.startingAt(timeSpan.start, period, modifier)
    }
  }

  case class StartAtEndOf(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends MoveSpanParse(timeSpanParse, periodParse) {
    def toUnspecifiedTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      TimeSpan(timeSpan.end, TimeSpan.unspecifiedEnd, period, modifier)
    }
    def toTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      TimeSpan.startingAt(timeSpan.end, period, modifier)
    }
  }

  case class EndAtStartOf(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends MoveSpanParse(timeSpanParse, periodParse) {
    def toUnspecifiedTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      TimeSpan(TimeSpan.unspecifiedStart, timeSpan.start, period, modifier)
    }
    def toTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      TimeSpan.endingAt(timeSpan.start, period, modifier)
    }
  }

  case class MoveEarlier(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends MoveSpanParse(timeSpanParse, periodParse) {
    def toUnspecifiedTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      TimeSpan(TimeSpan.unspecifiedStart, timeSpan.end, period, modifier)
    }
    def toTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      val start = period.subtractFrom(timeSpan.start)
      val end = period.subtractFrom(timeSpan.end)
      TimeSpan(start, end, timeSpan.period, modifier)
    }
  }

  case class MoveLater(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends MoveSpanParse(timeSpanParse, periodParse) {
    def toUnspecifiedTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      TimeSpan(timeSpan.start, TimeSpan.unspecifiedEnd, period, modifier)
    }
    def toTimeSpan(timeSpan: TimeSpan, period: Period, modifier: Modifier) = {
      val start = period.addTo(timeSpan.start)
      val end = period.addTo(timeSpan.end)
      TimeSpan(start, end, timeSpan.period, modifier)
    }
  }

  case class WithModifier(timeSpan: TimeSpanParse, modifier: Modifier) extends TimeSpanParse {
    def toTimeSpan(anchor: TimeSpan) = {
      timeSpan.toTimeSpan(anchor).copy(modifier = modifier)
    }
  }
}

sealed abstract class TimeSpanSetParse extends TemporalParse {
  def toTimeSpanSet: TimeSpanSet
}

object TimeSpanSetParse extends CanFail("[TimeSpanSet]") {

  def apply(tree: Tree)(implicit tokenParser: TokenParser): TimeSpanSetParse = tree match {
    case tree: Tree.NonTerminal if tree.rule.basicSymbol == "[TimeSpanSet]" => tree.children match {
      case tree :: Nil =>
        TimeSpanSetParse(tree)
      case Tree.Terminal("Simple") :: tree :: Nil =>
        Simple(FieldValueParse(tree).fieldValues)
      case _ => fail(tree)
    }
    case _ => fail(tree)
  }
  
  case class Simple(fields: Map[TemporalField, Int]) extends TimeSpanSetParse {
    def toTimeSpanSet = TimeSpanSet(fields)
  }
}
