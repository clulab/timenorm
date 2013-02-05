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

sealed abstract class TemporalParse

object TemporalParse {

  def apply(tree: Tree): TemporalParse = tree match {
    case tree: Tree.Terminal =>
      fail("[Temporal]", tree)
    case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
      case "[Number]" => NumberParse(tree)
      case "[Unit]" => UnitParse(tree)
      case "[Field]" => FieldParse(tree)
      case "[Period]" => PeriodParse(tree)
      case "[Anchor]" => AnchorParse(tree)
    }
  }

  private[timenorm] def fail[T](name: String, tree: Tree): T = {
    throw new UnsupportedOperationException(
      "Don't know how to parse %s from %s".format(name, tree match {
        case tree: Tree.Terminal => tree.token
        case tree: Tree.NonTerminal => tree.rule.basicSymbol + " -> " + tree.children.map {
          case child: Tree.Terminal => child.token
          case child: Tree.NonTerminal => child.rule.symbol
        }.mkString(" ")
      }))
  }
}

case class NumberParse(value: Int) extends TemporalParse
object NumberParse {
  def apply(tree: Tree): NumberParse = tree match {
    case tree: Tree.Terminal =>
      NumberParse(tree.token.toInt)
    case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
      case "[Number]" => tree.children match {
        case tree :: Nil =>
          NumberParse(tree)
        case _ =>
          TemporalParse.fail("Number", tree)
      }
      case _ =>
        TemporalParse.fail("Number", tree)
    }
  }
}

case class UnitParse(value: ChronoUnit) extends TemporalParse
object UnitParse {
  def apply(tree: Tree): UnitParse = tree match {
    case tree: Tree.Terminal =>
      UnitParse(ChronoUnit.valueOf(tree.token))
    case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
      case "[Unit]" => tree.children match {
        case tree :: Nil =>
          UnitParse(tree)
        case _ =>
          TemporalParse.fail("Unit", tree)
      }
      case _ =>
        TemporalParse.fail("Unit", tree)
    }
  }
}

case class FieldParse(name: ChronoField, value: Int) extends TemporalParse
object FieldParse {
  def apply(tree: Tree): FieldParse = tree match {
    case tree: Tree.Terminal =>
      TemporalParse.fail("Field", tree)
    case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
      case "[Field]" => tree.children match {
        case tree :: Nil =>
          FieldParse(tree)
        case (tree: Tree.Terminal) :: number :: Nil =>
          FieldParse(ChronoField.valueOf(tree.token), NumberParse(number).value)
        case _ =>
          TemporalParse.fail("Field", tree)
      }
      case _ =>
        TemporalParse.fail("Field", tree)
    }
  }
}

sealed abstract class PeriodParse extends TemporalParse {
  def toPeriod: Period
}

object PeriodParse {

  def apply(tree: Tree): PeriodParse = tree match {
    case unit: Tree.Terminal =>
      SimplePeriod(1, UnitParse(unit).value)
    case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
      case "[Period]" => tree.children match {
        case unit :: Nil =>
          SimplePeriod(1, UnitParse(unit).value)
        case amount :: unit :: Nil =>
          SimplePeriod(NumberParse(amount).value, UnitParse(unit).value)
        case Tree.Terminal("Sum") :: period1 :: period2 :: Nil =>
          Plus(PeriodParse(period1), PeriodParse(period2))
        case _ =>
          TemporalParse.fail("Period", tree)
      }
      case _ =>
        TemporalParse.fail("Period", tree)
    }
  }

  case class SimplePeriod(amount: Int, unit: ChronoUnit) extends PeriodParse {
    def toPeriod = Period(Map(unit -> amount))
  }

  case class Plus(periodParse1: PeriodParse, periodParse2: PeriodParse) extends PeriodParse {
    def toPeriod = periodParse1.toPeriod + periodParse2.toPeriod
  }

  case class Minus(periodParse1: PeriodParse, periodParse2: PeriodParse) extends PeriodParse {
    def toPeriod = periodParse1.toPeriod - periodParse2.toPeriod
  }
}

sealed abstract class AnchorParse extends TemporalParse {
  def toDateTime(anchor: ZonedDateTime): DateTime
}

object AnchorParse {

  def apply(tree: Tree): AnchorParse = tree match {
    case Tree.Terminal("TODAY") =>
      Today
    case Tree.Terminal("PAST") =>
      Past
    case Tree.Terminal("PRESENT") =>
      Present
    case Tree.Terminal("FUTURE") =>
      Future
    case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
      case "[Anchor]" => tree.children match {
        case Tree.Terminal("PAST") :: Nil =>
          Past
        case Tree.Terminal("PRESENT") :: Nil =>
          Present
        case Tree.Terminal("FUTURE") :: Nil =>
          Future
        case Tree.Terminal("TODAY") :: Nil =>
          Today
        case Tree.Terminal("CurrentField") :: Tree.Terminal(fieldName) :: Nil =>
          CurrentField(ChronoField.valueOf(fieldName))
        case Tree.Terminal("Date") :: year :: month :: day :: Nil =>
          Date(FieldParse(year).value, FieldParse(month).value, FieldParse(day).value)
        case Tree.Terminal("Next") :: tail =>
          Next(this.toFieldNameValuePairs(tail).toMap)
        case Tree.Terminal("Previous") :: tail =>
          Previous(this.toFieldNameValuePairs(tail).toMap)
        case Tree.Terminal("CurrentOrPrevious") :: tail =>
          CurrentOrPrevious(this.toFieldNameValuePairs(tail).toMap)
        case Tree.Terminal("Closest") :: tail =>
          Closest(this.toFieldNameValuePairs(tail).toMap)
        case Tree.Terminal("Plus") :: anchor :: period :: Nil =>
          Plus(AnchorParse(anchor), PeriodParse(period))
        case Tree.Terminal("Minus") :: anchor :: period :: Nil =>
          Minus(AnchorParse(anchor), PeriodParse(period))
        case _ =>
          TemporalParse.fail("Anchor", tree)
      }
      case _ =>
        TemporalParse.fail("Anchor", tree)
    }
    case _ =>
      TemporalParse.fail("Anchor", tree)
  }

  private def toFieldNameValuePairs(trees: List[Tree]): List[(ChronoField, Int)] = {
    trees.map {
      case tree: Tree.NonTerminal =>
        val field = FieldParse(tree)
        (field.name, field.value)
      case tree: Tree.Terminal =>
        TemporalParse.fail("Field", tree)
    }
  }

  case object Today extends AnchorParse {
    def toDateTime(anchor: ZonedDateTime) = {
      DateTime(anchor, ChronoUnit.DAYS, ChronoUnit.DAYS)
    }
  }
  
  case object Past extends AnchorParse {
    def toDateTime(anchor: ZonedDateTime) = {
      new DateTime(anchor, ChronoUnit.FOREVER, ChronoUnit.FOREVER) {
        override def toTimeMLValue(unit: TemporalUnit) = "PAST_REF"
      }
    }
  }

  case object Present extends AnchorParse {
    def toDateTime(anchor: ZonedDateTime) = {
      new DateTime(anchor, ChronoUnit.SECONDS, ChronoUnit.FOREVER) {
        override def toTimeMLValue(unit: TemporalUnit) = "PRESENT_REF"
      }
    }
  }

  case object Future extends AnchorParse {
    def toDateTime(anchor: ZonedDateTime) = {
      new DateTime(anchor, ChronoUnit.FOREVER, ChronoUnit.FOREVER) {
        override def toTimeMLValue(unit: TemporalUnit) = "FUTURE_REF"
      }
    }
  }

  case class CurrentField(field: ChronoField) extends AnchorParse {
    def toDateTime(anchor: ZonedDateTime) = {
      DateTime(anchor, field.getBaseUnit, field.getBaseUnit)
    }
  }

  case class Date(year: Int, month: Int, day: Int) extends AnchorParse {
    def toDateTime(anchor: ZonedDateTime) = {
      val dateTime = anchor.withYear(year).withMonth(month).withDayOfMonth(day)
      DateTime(dateTime, ChronoUnit.DAYS, ChronoUnit.DAYS)
    }
  }
  
  abstract class FieldSearchingAnchorParse(fields: Map[ChronoField, Int]) extends AnchorParse {
    
    val minUnit = fields.keySet.map(_.getBaseUnit).minBy(_.getDuration)

    def searchFrom(dateTime: ZonedDateTime, step: (ZonedDateTime, TemporalUnit) => ZonedDateTime): ZonedDateTime = {
      var curr = dateTime
      while (fields.exists { case (field, value) => curr.get(field) != value }) {
        curr = step(curr, this.minUnit)
      }
      curr
    }
  }

  abstract class DirectedFieldSearchingAnchorParse(
      fields: Map[ChronoField, Int],
      stepFirst: (ZonedDateTime, TemporalUnit) => ZonedDateTime,
      step: (ZonedDateTime, TemporalUnit) => ZonedDateTime)
      extends FieldSearchingAnchorParse(fields) {

    def toDateTime(dateTime: ZonedDateTime) = {
      val adjustedDateTime = this.searchFrom(this.stepFirst(dateTime, this.minUnit), this.step)
      DateTime(adjustedDateTime, this.minUnit, this.minUnit)
    }
  }
  
  case class Next(fields: Map[ChronoField, Int])
    extends DirectedFieldSearchingAnchorParse(fields, _.plus(1, _), _.plus(1, _))

  case class Previous(fields: Map[ChronoField, Int])
    extends DirectedFieldSearchingAnchorParse(fields, _.minus(1, _), _.minus(1, _))
  
  case class CurrentOrPrevious(fields: Map[ChronoField, Int])
    extends DirectedFieldSearchingAnchorParse(fields, _.minus(0, _), _.minus(1, _))

  case class Closest(fields: Map[ChronoField, Int]) extends FieldSearchingAnchorParse(fields) {
    def toDateTime(dateTime: ZonedDateTime) = {
      val prev = this.searchFrom(dateTime, _.minus(1, _))
      val next = this.searchFrom(dateTime, _.plus(1, _))
      val distToPrev = prev.periodUntil(dateTime, this.minUnit)
      val distToNext = dateTime.periodUntil(next, this.minUnit)
      val adjustedDateTime = if (distToPrev < distToNext) prev else next
      DateTime(adjustedDateTime, this.minUnit, this.minUnit)
    }
  }

  abstract class PeriodAnchorParse(
      anchorParse: AnchorParse,
      periodParse: PeriodParse,
      adjust: (ZonedDateTime, Int, TemporalUnit) => ZonedDateTime) extends AnchorParse {

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

  case class Plus(anchor: AnchorParse, period: PeriodParse)
    extends PeriodAnchorParse(anchor, period, _.plus(_, _))

  case class Minus(anchor: AnchorParse, period: PeriodParse)
    extends PeriodAnchorParse(anchor, period, _.minus(_, _))
}

sealed abstract class ModParse extends TemporalParse
object ModParse {
  case object Exact extends ModParse
  case object Before extends ModParse
  case object After extends ModParse
  case object OnOrBefore extends ModParse
  case object OnOrAfter extends ModParse
  case object LessThan extends ModParse
  case object MoreThan extends ModParse
  case object EqualOrLess extends ModParse
  case object EqualOrMore extends ModParse
  case object Start extends ModParse
  case object Mid extends ModParse
  case object End extends ModParse
  case object Approx extends ModParse
}