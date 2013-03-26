package info.bethard.timenorm

import scala.collection.immutable.Seq

import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneId
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit

import info.bethard.timenorm.SynchronousParser.Tree

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
    case "YEAR_OF_CENTURY" => YEAR_OF_CENTURY
    case "CENTURY" => CENTURY
    case _ => ChronoField.valueOf(token)
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
      case tree @ Tree.NonTerminal("[Period]", _, _, _) =>
        PeriodParse(tree)
      case tree @ Tree.NonTerminal("[TimeSpan]", _, _, _) =>
        TimeSpanParse(tree)
      case _ => fail(tree)
    }
  }
}

case class IntParse(value: Int)
object IntParse extends CanFail("[Int]") {
  def apply(tree: Tree)(implicit tokenParser: TokenParser): IntParse = tree match {
    case Tree.Terminal(number) =>
      IntParse(tokenParser.toInt(number))
    case Tree.NonTerminal("[Int]", _, tree :: Nil, _) =>
      IntParse(tree)
    case _ => fail(tree)
  }
}

case class UnitParse(value: TemporalUnit)
object UnitParse extends CanFail("[Unit]") {
  def apply(tree: Tree)(implicit tokenParser: TokenParser): UnitParse = tree match {
    case Tree.Terminal(unit) =>
      UnitParse(tokenParser.toTemporalUnit(unit))
    case Tree.NonTerminal("[Unit]", _, tree :: Nil, _) =>
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
    case Tree.NonTerminal("[FieldValue]", _, Tree.Terminal(field) :: number :: Nil, _) =>
      FieldValueParse(Map(tokenParser.toTemporalField(field) -> IntParse(number).value))
    case Tree.NonTerminal("[FieldValue]", _, children, _) =>
      FieldValueParse(children.map(FieldValueParse.apply).map(_.fieldValues).flatten.toMap)
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
    case Tree.NonTerminal(_, "[Period]", tree :: Nil, _) =>
      PeriodParse(tree)
    case Tree.NonTerminal(_, "[Period:Simple]", unit :: Nil, _) =>
      Simple(1, UnitParse(unit).value)
    case Tree.NonTerminal(_, "[Period:Simple]", amount :: unit :: Nil, _) =>
      Simple(IntParse(amount).value, UnitParse(unit).value)
    case Tree.NonTerminal(_, "[Period:Unspecified]", unit :: Nil, _) =>
      Unspecified(UnitParse(unit).value)
    case Tree.NonTerminal(_, "[Period:Fractional]", numerator :: denominator :: unit :: Nil, _) =>
      Fractional(IntParse(numerator).value, IntParse(denominator).value, UnitParse(unit).value)
    case Tree.NonTerminal(_, "[Period:Fractional]", whole :: numerator :: denominator :: unit :: Nil, _) =>
      val denominatorValue = IntParse(denominator).value
      Fractional(IntParse(whole).value * denominatorValue + IntParse(numerator).value, denominatorValue, UnitParse(unit).value)
    case Tree.NonTerminal(_, "[Period:Sum]", children, _) =>
      Sum(children.map(PeriodParse.apply))
    case Tree.NonTerminal(_, "[Period:WithModifier]", period :: Tree.Terminal(modifier) :: Nil, _) =>
      WithModifier(PeriodParse(period), Modifier.valueOf(modifier))
    case Tree.NonTerminal(_, "[Period:WithQuantifier]", period :: Tree.Terminal(quantifier) :: Nil, _) =>
      WithQuantifier(PeriodParse(period), Quantifier.valueOf(quantifier))
    case Tree.NonTerminal(_, "[Period:WithFrequency]", period :: times :: Nil, _) =>
      WithFrequency(PeriodParse(period), Frequency(IntParse(times).value))
    case Tree.NonTerminal(_, "[Period:WithFrequency]", period :: times :: unit :: Nil, _) =>
      WithFrequency(PeriodParse(period), Frequency(IntParse(times).value, Some(UnitParse(unit).value)))
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
  
  case class WithQuantifier(period: PeriodParse, quantifier: Quantifier) extends PeriodParse {
    def toPeriod = period.toPeriod.copy(quantifier = quantifier)
  }
  
  case class WithFrequency(period: PeriodParse, frequency: Frequency) extends PeriodParse {
    def toPeriod = period.toPeriod.copy(frequency = frequency)
  }
}

sealed abstract class TimeSpanParse extends TemporalParse {
  def toTimeSpan(anchor: ZonedDateTime): TimeSpan
}

object TimeSpanParse extends CanFail("[TimeSpan]") {

  def apply(tree: Tree)(implicit tokenParser: TokenParser): TimeSpanParse = tree match {
    case Tree.Terminal("PAST") =>
      Past
    case Tree.Terminal("PRESENT") =>
      Present
    case Tree.Terminal("FUTURE") =>
      Future
    case Tree.NonTerminal(_, "[TimeSpan]", tree :: Nil, _) =>
      TimeSpanParse(tree)
    case Tree.NonTerminal(_, "[TimeSpan:Simple]", (tree: Tree.Terminal) :: Nil, _) =>
      TimeSpanParse(tree)
    case Tree.NonTerminal(_, "[TimeSpan:FindAbsolute]", tree :: Nil, _) =>
      FindAbsolute(FieldValueParse(tree).fieldValues)
    case Tree.NonTerminal(_, "[TimeSpan:FindLater]", tree :: Nil, _) =>
      FindLater(FieldValueParse(tree).fieldValues)
    case Tree.NonTerminal(_, "[TimeSpan:FindEarlier]", tree :: Nil, _) =>
      FindEarlier(FieldValueParse(tree).fieldValues)
    case Tree.NonTerminal(_, "[TimeSpan:FindCurrentOrLater]", tree :: Nil, _) =>
      FindCurrentOrLater(FieldValueParse(tree).fieldValues)
    case Tree.NonTerminal(_, "[TimeSpan:FindCurrentOrEarlier]", tree :: Nil, _) =>
      FindCurrentOrEarlier(FieldValueParse(tree).fieldValues)
    case Tree.NonTerminal(_, "[TimeSpan:FindEnclosing]", time :: unit :: Nil, _) =>
      FindEnclosing(TimeSpanParse(time), UnitParse(unit).value)
    case Tree.NonTerminal(_, "[TimeSpan:StartAtStartOf]", time :: period :: Nil, _) =>
      StartAtStartOf(TimeSpanParse(time), PeriodParse(period))
    case Tree.NonTerminal(_, "[TimeSpan:StartAtStartOf+FindEnclosing]", time :: period :: Nil, _) =>
      StartAtStartOf(TimeSpanParse(time), PeriodParse(period)).withFindEnclosing()
    case Tree.NonTerminal(_, "[TimeSpan:StartAtEndOf]", time :: period :: Nil, _) =>
      StartAtEndOf(TimeSpanParse(time), PeriodParse(period))
    case Tree.NonTerminal(_, "[TimeSpan:StartAtEndOf+FindEnclosing]", time :: period :: Nil, _) =>
      StartAtEndOf(TimeSpanParse(time), PeriodParse(period)).withFindEnclosing()
    case Tree.NonTerminal(_, "[TimeSpan:EndAtStartOf]", time :: period :: Nil, _) =>
      EndAtStartOf(TimeSpanParse(time), PeriodParse(period))
    case Tree.NonTerminal(_, "[TimeSpan:EndAtStartOf+FindEnclosing]", time :: period :: Nil, _) =>
      EndAtStartOf(TimeSpanParse(time), PeriodParse(period)).withFindEnclosing()
    case Tree.NonTerminal(_, "[TimeSpan:MoveEarlier]", time :: period :: Nil, _) =>
      MoveEarlier(TimeSpanParse(time), PeriodParse(period))
    case Tree.NonTerminal(_, "[TimeSpan:MoveEarlier+FindEnclosing]", time :: period :: Nil, _) =>
      MoveEarlier(TimeSpanParse(time), PeriodParse(period)).withFindEnclosing()
    case Tree.NonTerminal(_, "[TimeSpan:MoveLater]", time :: period :: Nil, _) =>
      MoveLater(TimeSpanParse(time), PeriodParse(period))
    case Tree.NonTerminal(_, "[TimeSpan:MoveLater+FindEnclosing]", time :: period :: Nil, _) =>
      MoveLater(TimeSpanParse(time), PeriodParse(period)).withFindEnclosing()
    case Tree.NonTerminal(_, "[TimeSpan:WithModifier]", time :: Tree.Terminal(modifier) :: Nil, _) =>
      WithModifier(TimeSpanParse(time), Modifier.valueOf(modifier))
    case _ => fail(tree)
  }

  case object Past extends TimeSpanParse {
    def toTimeSpan(anchor: ZonedDateTime) = {
      new TimeSpan(
          ZonedDateTime.of(LocalDateTime.MIN, ZoneId.of("Z")),
          anchor,
          Period.infinite,
          Modifier.Approx) {
        override def timeMLValueOption = Some("PAST_REF")
      }
    }
  }

  case object Present extends TimeSpanParse {
    def toTimeSpan(anchor: ZonedDateTime) = {
      new TimeSpan(anchor, anchor, Period.empty, Modifier.Exact) {
        override def timeMLValueOption = Some("PRESENT_REF")
      }
    }
  }

  case object Future extends TimeSpanParse {
    def toTimeSpan(anchor: ZonedDateTime) = {
      new TimeSpan(
          anchor,
          ZonedDateTime.of(LocalDateTime.MAX, ZoneId.of("Z")),
          Period.infinite,
          Modifier.Approx) {
        override def timeMLValueOption = Some("FUTURE_REF")
      }
    }
  }

  abstract class FieldBasedTimeSpanParse(fields: Map[TemporalField, Int]) extends TimeSpanParse {
    val minUnit = fields.keySet.map(_.getBaseUnit).minBy(_.getDuration)
  }

  case class FindAbsolute(fields: Map[TemporalField, Int]) extends FieldBasedTimeSpanParse(fields) {
    def toTimeSpan(anchor: ZonedDateTime) = {
      val begin = fields.foldLeft(anchor) {
        case (time, (field, value)) => time.`with`(field, value)
      }
      val period = Period(Map(this.minUnit -> 1), Modifier.Exact)
      TimeSpan.startingAt(TimeSpan.truncate(begin, this.minUnit), period, Modifier.Exact)
    }
  }

  abstract class FieldSearchingTimeSpanParse(fields: Map[TemporalField, Int]) extends FieldBasedTimeSpanParse(fields) {

    def searchFrom(dateTime: ZonedDateTime, step: (ZonedDateTime, TemporalUnit) => ZonedDateTime): ZonedDateTime = {
      val searchField = this.fields.keySet.minBy(_.getBaseUnit.getDuration)

      // search by base units for partial ranges (e.g. search by hours, not "mornings")
      var searchUnit = searchField.getBaseUnit match {
        case partialRange: PartialRange => partialRange.field.getBaseUnit
        case unit => unit
      }

      // if the field's range is fixed and the range unit is not estimated,
      // then we can move by range units once we satisfy the base unit
      var canSwitchToRange = searchField.range().isFixed() && !searchField.getRangeUnit().isDurationEstimated()

      // one step at a time, search for a time that satisfies the field requirements
      var curr = dateTime
      while (!this.satisfiesFieldValues(curr)) {
        curr = step(curr, searchUnit)

        // if we've satisfied the search field's base unit, start moving by range units
        if (canSwitchToRange && curr.get(searchField) == this.fields(searchField)) {
          searchUnit = searchField.getRangeUnit()
          canSwitchToRange = false
        }
      }
      TimeSpan.truncate(curr, this.minUnit)
    }
    
    private def satisfiesFieldValues(dateTime: ZonedDateTime): Boolean = {
      // must match all field values
      if (this.fields.exists { case (field, value) => dateTime.get(field) != value }) {
        false
      }
      // must still match all field values after truncation
      else {
        val truncatedDateTime = TimeSpan.truncate(dateTime, this.minUnit)
        fields.forall { case (field, value) => truncatedDateTime.get(field) == value }
      }
    }
  }
  
  abstract class DirectedFieldSearchingTimeSpanParse(
    fields: Map[TemporalField, Int],
    stepFirst: (ZonedDateTime, TemporalUnit) => ZonedDateTime,
    step: (ZonedDateTime, TemporalUnit) => ZonedDateTime)
      extends FieldSearchingTimeSpanParse(fields) {

    def toTimeSpan(anchor: ZonedDateTime) = {
      val begin = this.searchFrom(this.stepFirst(anchor, this.minUnit), this.step)
      val period = Period(Map(this.minUnit -> 1), Modifier.Exact)
      TimeSpan.startingAt(begin, period, Modifier.Exact)
    }
  }

  case class FindLater(fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(fields, _.plus(1, _), _.plus(1, _))

  case class FindCurrentOrLater(fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(fields, _.plus(0, _), _.plus(1, _))

  case class FindEarlier(fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(fields, _.minus(1, _), _.minus(1, _))

  case class FindCurrentOrEarlier(fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(fields, _.minus(0, _), _.minus(1, _))

  case class FindEnclosing(timeSpanParse: TimeSpanParse, unit: TemporalUnit) extends TimeSpanParse {
    def toTimeSpan(anchor: ZonedDateTime) = {
      val timeSpan = timeSpanParse.toTimeSpan(anchor)
      if (timeSpan.period > unit) {
        throw new IllegalArgumentException("%s is larger than 1 %s".format(timeSpan, unit))
      }
      var start = TimeSpan.truncate(timeSpan.start, unit)
      if (start.isAfter(timeSpan.start)) {
        start = start.minus(1, unit)
      }
      val period = Period(Map(unit -> 1), Modifier.Exact)
      TimeSpan.startingAt(start, period, timeSpan.modifier & period.modifier)
    }
  }
  
  trait TimeSpanPeriodParse extends TimeSpanParse {
    val timeSpanParse: TimeSpanParse
    val periodParse: PeriodParse
    def withFindEnclosing(): TimeSpanPeriodParse
    
    protected def encloseTimeSpan() = {
      val unit = this.periodParse.toPeriod.unitAmounts.keySet.minBy(_.getDuration)
      FindEnclosing(this.timeSpanParse, unit)
    }
  }

  case class StartAtStartOf(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends TimeSpanPeriodParse {
    def withFindEnclosing() = this.copy(timeSpanParse = this.encloseTimeSpan())
    def toTimeSpan(anchor: ZonedDateTime) = {
      val timeSpan = timeSpanParse.toTimeSpan(anchor)
      val period = periodParse.toPeriod
      TimeSpan.startingAt(timeSpan.start, period, timeSpan.modifier & period.modifier)
    }
  }

  case class StartAtEndOf(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends TimeSpanPeriodParse {
    def withFindEnclosing() = this.copy(timeSpanParse = this.encloseTimeSpan())
    def toTimeSpan(anchor: ZonedDateTime) = {
      val timeSpan = timeSpanParse.toTimeSpan(anchor)
      val period = periodParse.toPeriod
      TimeSpan.startingAt(timeSpan.end, period, timeSpan.modifier & period.modifier)
    }
  }

  case class EndAtStartOf(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends TimeSpanPeriodParse {
    def withFindEnclosing() = this.copy(timeSpanParse = this.encloseTimeSpan())
    def toTimeSpan(anchor: ZonedDateTime) = {
      val timeSpan = timeSpanParse.toTimeSpan(anchor)
      val period = periodParse.toPeriod
      TimeSpan.endingAt(timeSpan.start, period, timeSpan.modifier & period.modifier)
    }
  }

  case class MoveEarlier(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends TimeSpanPeriodParse {
    def withFindEnclosing() = this.copy(timeSpanParse = this.encloseTimeSpan())
    def toTimeSpan(anchor: ZonedDateTime) = {
      val timeSpan = timeSpanParse.toTimeSpan(anchor)
      val period = periodParse.toPeriod
      val start = period.subtractFrom(timeSpan.start)
      val end = period.subtractFrom(timeSpan.end)
      TimeSpan(start, end, timeSpan.period, timeSpan.modifier & period.modifier)
    }
  }

  case class MoveLater(timeSpanParse: TimeSpanParse, periodParse: PeriodParse)
  extends TimeSpanPeriodParse {
    def withFindEnclosing() = this.copy(timeSpanParse = this.encloseTimeSpan())
    def toTimeSpan(anchor: ZonedDateTime) = {
      val timeSpan = timeSpanParse.toTimeSpan(anchor)
      val period = periodParse.toPeriod
      val start = period.addTo(timeSpan.start)
      val end = period.addTo(timeSpan.end)
      TimeSpan(start, end, timeSpan.period, timeSpan.modifier & period.modifier)
    }
  }

  case class WithModifier(timeSpan: TimeSpanParse, modifier: Modifier) extends TimeSpanParse {
    def toTimeSpan(anchor: ZonedDateTime) = {
      timeSpan.toTimeSpan(anchor).copy(modifier = modifier)
    }
  }
}
