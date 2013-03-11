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
import org.threeten.bp.Instant
import org.threeten.bp.ZoneId
import org.threeten.bp.temporal.TemporalField

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
        case tree: Tree.NonTerminal => tree.rule.symbol + " -> " + tree.children.map {
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
    case tree @ Tree.NonTerminal("[TimeSpan]", _, _, _) =>
      TimeSpanParse(tree)
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

case class FieldValueParse(name: TemporalField, value: Int)
object FieldValueParse extends CanFail("[FieldValue]") with (Tree => FieldValueParse) {
  def apply(tree: Tree): FieldValueParse = tree match {
    case Tree.NonTerminal("[FieldValue]", _, tree :: Nil, _) =>
      FieldValueParse(tree)
    case Tree.NonTerminal("[FieldValue]", _, Tree.Terminal(field) :: number :: Nil, _) =>
      FieldValueParse(TemporalFields.valueOf(field), NumberParse(number).value)
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
    case Tree.NonTerminal(_, "[Period:WithModifier]", period :: Tree.Terminal(modifier) :: Nil, _) =>
      WithModifier(PeriodParse(period), Modifier.valueOf(modifier))
    case _ => fail(tree)
  }

  case class Simple(amount: Int, unit: ChronoUnit) extends PeriodParse {
    def toPeriod = Period(Map(unit -> amount), Modifier.Exact)
  }

  case class Sum(periods: Seq[PeriodParse]) extends PeriodParse {
    def toPeriod = periods.foldLeft(Period.empty)(_ + _.toPeriod)
  }

  case class WithModifier(period: PeriodParse, modifier: Modifier) extends PeriodParse {
    def toPeriod = period.toPeriod.copy(modifier = modifier)
  }
}

sealed abstract class TimeSpanParse extends TemporalParse {
  def toTimeSpan(anchor: ZonedDateTime): TimeSpan
}

object TimeSpanParse extends CanFail("[TimeSpan]") with (Tree => TimeSpanParse) {

  def apply(tree: Tree): TimeSpanParse = tree match {
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
    case Tree.NonTerminal(_, "[TimeSpan:FindAbsolute]", children, _) =>
      FindAbsolute(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[TimeSpan:FindLater]", children, _) =>
      FindLater(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[TimeSpan:FindEarlier]", children, _) =>
      FindEarlier(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[TimeSpan:FindCurrentOrEarlier]", children, _) =>
      FindCurrentOrEarlier(this.toFieldValues(children))
    case Tree.NonTerminal(_, "[TimeSpan:FindEnclosing]", time :: unit :: Nil, _) =>
      FindEnclosing(TimeSpanParse(time), UnitParse(unit).value)
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

  private def toFieldValues(trees: List[Tree]): Map[TemporalField, Int] = {
    trees.map(FieldValueParse).map(field => (field.name, field.value)).toMap
  }

  case object Past extends TimeSpanParse {
    def toTimeSpan(anchor: ZonedDateTime) = {
      new TimeSpan(null, anchor, null, Modifier.Approx) {
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
      new TimeSpan(anchor, null, null, Modifier.Approx) {
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
      var curr = dateTime
      while (fields.exists { case (field, value) => curr.get(field) != value }) {
        curr = step(curr, this.minUnit)
      }
      TimeSpan.truncate(curr, this.minUnit)
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

  case class FindEarlier(fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(fields, _.minus(1, _), _.minus(1, _))

  case class FindCurrentOrEarlier(fields: Map[TemporalField, Int])
    extends DirectedFieldSearchingTimeSpanParse(fields, _.minus(0, _), _.minus(1, _))

  case class FindEnclosing(timeSpanParse: TimeSpanParse, unit: TemporalUnit) extends TimeSpanParse {
    import ChronoUnit._
    import ChronoField._
    
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
