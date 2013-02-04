package info.bethard.timenorm
import scala.collection.immutable.Seq

import org.threeten.bp.Duration
import org.threeten.bp.ZonedDateTime
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.{ Temporal => JTemporal }
import org.threeten.bp.temporal.TemporalAdder
import org.threeten.bp.temporal.TemporalSubtractor

import info.bethard.timenorm.SynchronousParser.Tree

class TemporalParser(grammar: SynchronousGrammar) {
  
  val parser = new SynchronousParser(grammar)
  
  def parseAll(sourceTokens: Seq[String]): Seq[Temporal] = {
    this.parser.parseAll(sourceTokens).map(Temporal.fromParse)
  }
  
  def parseAll(sourceTokens: Array[String]): Array[Temporal] = {
    this.parseAll(sourceTokens.toIndexedSeq).toArray
  }
  
  def parseAll(sourceText: String): Array[Temporal] = {
    this.parseAll(sourceText.split("\\s*\\b\\s*").filter(!_.matches("\\s*")))
  }
}

sealed abstract class Temporal

object Temporal {

  def fromParse(tree: Tree): Temporal = tree match {
    case tree: Tree.Terminal =>
      fail("[Temporal]", tree)
    case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
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
        case tree: Tree.NonTerminal => tree.rule.basicSymbol + " -> " + tree.children.map {
          case child: Tree.Terminal => child.token
          case child: Tree.NonTerminal => child.rule.symbol
        }.mkString(" ")
      }))
  }

  case class Number(value: Int) extends Temporal
  object Number {
    def fromParse(tree: Tree): Number = tree match {
      case tree: Tree.Terminal =>
        Number(tree.token.toInt)
      case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
        case "[Number]" => tree.children match {
          case tree :: Nil =>
            Number.fromParse(tree)
          case _ =>
            fail("Number", tree)
        }
        case _ =>
          fail("Number", tree)
      }
    }
  }

  case class Unit(value: ChronoUnit) extends Temporal
  object Unit {
    def fromParse(tree: Tree): Unit = tree match {
      case tree: Tree.Terminal =>
        Unit(ChronoUnit.valueOf(tree.token))
      case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
        case "[Unit]" => tree.children match {
          case tree :: Nil =>
            Unit.fromParse(tree)
          case _ =>
            fail("Unit", tree)
        }
        case _ =>
          fail("Unit", tree)
      }
    }
  }

  case class Field(name: ChronoField, value: Int) extends Temporal
  object Field {
    def fromParse(tree: Tree): Field = tree match {
      case tree: Tree.Terminal =>
        fail("Field", tree)
      case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
        case "[Field]" => tree.children match {
          case tree :: Nil =>
            Field.fromParse(tree)
          case (tree: Tree.Terminal) :: number :: Nil =>
            Field(ChronoField.valueOf(tree.token), Number.fromParse(number).value)
          case _ =>
            fail("Field", tree)
        }
        case _ =>
          fail("Field", tree)
      }
    }
  }

  sealed abstract class Anchor extends Temporal {

    def minDuration: Duration

    def toDateTime(anchor: ZonedDateTime): ZonedDateTime

    def toTimeMLValue(anchor: ZonedDateTime): String = {
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
      ChronoField.DAY_OF_MONTH -> "-%02d",
      ChronoField.HOUR_OF_DAY -> "T%02d",
      ChronoField.MINUTE_OF_HOUR -> ":%02d",
      ChronoField.SECOND_OF_MINUTE -> ":%02d")
  }

  object Anchor {

    def fromParse(tree: Tree): Anchor = tree match {
      case Tree.Terminal("TODAY") =>
        Temporal.Anchor.Today
      case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
        case "[Anchor]" => tree.children match {
          case Tree.Terminal("TODAY") :: Nil =>
            Temporal.Anchor.Today
          case Tree.Terminal("Date") :: year :: month :: day :: Nil =>
            Date(Field.fromParse(year).value, Field.fromParse(month).value, Field.fromParse(day).value)
          case Tree.Terminal("Next") :: tail =>
            Next(this.toFieldNameValuePairs(tail).toMap)
          case Tree.Terminal("Previous") :: tail =>
            Previous(this.toFieldNameValuePairs(tail).toMap)
          case Tree.Terminal("Closest") :: tail =>
            Closest(this.toFieldNameValuePairs(tail).toMap)
          case Tree.Terminal("Plus") :: anchor :: period :: Nil =>
            Plus(Anchor.fromParse(anchor), Period.fromParse(period))
          case Tree.Terminal("Minus") :: anchor :: period :: Nil =>
            Minus(Anchor.fromParse(anchor), Period.fromParse(period))
          case _ =>
            fail("Anchor", tree)
        }
        case _ =>
          fail("Anchor", tree)
      }
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

      val minDuration = ChronoUnit.DAYS.getDuration

      def toDateTime(anchor: ZonedDateTime) = anchor
    }

    case object Now extends Anchor {

      val minDuration = ChronoUnit.SECONDS.getDuration

      def toDateTime(anchor: ZonedDateTime) = anchor
      
      override def toTimeMLValue(anchor: ZonedDateTime) = "PRESENT_REF"
    }

    case class Date(year: Int, month: Int, day: Int) extends Anchor {

      val minDuration = ChronoUnit.DAYS.getDuration

      def toDateTime(anchor: ZonedDateTime) = {
        anchor.withYear(year).withMonth(month).withDayOfMonth(day)
      }
    }

    case class Next(fields: Map[ChronoField, Int]) extends Anchor {

      val minDuration = fields.keySet.map(_.getBaseUnit.getDuration).min

      def toDateTime(anchor: ZonedDateTime) = {
        anchor.plus(new FollowingAdjuster(fields))
      }
    }

    case class Previous(fields: Map[ChronoField, Int]) extends Anchor {

      val minDuration = fields.keySet.map(_.getBaseUnit.getDuration).min

      def toDateTime(anchor: ZonedDateTime) = {
        anchor.minus(new PreviousAdjuster(fields))
      }
    }
    
    case class Closest(fields: Map[ChronoField, Int]) extends Anchor {
      
      val minDuration = fields.keySet.map(_.getBaseUnit.getDuration).min

      def toDateTime(anchor: ZonedDateTime) = {
        anchor.plus(new ClosestAdjuster(fields))
      }
    }

    case class Plus(anchor: Anchor, period: Period) extends Anchor {

      val minDuration = anchor.minDuration 

      def toDateTime(anchorDateTime: ZonedDateTime) = {
        var result = anchor.toDateTime(anchorDateTime)
        for ((unit, amount) <- period.toUnitCounts) {
          result = result.plus(amount, unit)
        }
        result
      }
    }

    case class Minus(anchor: Anchor, period: Period) extends Anchor {

      val minDuration = anchor.minDuration

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
        var curr = temporal
        while (constraints.exists { case (field, value) => curr.get(field) != value }) {
          curr = adjust(curr)
        }
        curr
      }
    }

    private class PreviousAdjuster(constraints: Map[ChronoField, Int]) extends SearchingAdjuster(constraints) with TemporalSubtractor {
      def subtractFrom(temporal: JTemporal): JTemporal = {
        this.adjustInto(temporal.minus(1, this.unit), _.minus(1, this.unit))
      }
    }

    private class FollowingAdjuster(constraints: Map[ChronoField, Int]) extends SearchingAdjuster(constraints) with TemporalAdder {
      def addTo(temporal: JTemporal): JTemporal = {
        this.adjustInto(temporal.plus(1, this.unit), _.plus(1, this.unit))
      }
    }
    
    private class ClosestAdjuster(constraints: Map[ChronoField, Int]) extends SearchingAdjuster(constraints) with TemporalAdder {
      def addTo(temporal: JTemporal): JTemporal = {
        val prev = this.adjustInto(temporal, _.minus(1, this.unit))
        val next = this.adjustInto(temporal, _.plus(1, this.unit))
        if (prev.periodUntil(temporal, this.unit) < temporal.periodUntil(next, this.unit)) {
          prev
        } else {
          next
        }
      }
    }
  }

  sealed abstract class Period extends Temporal {

    def toUnitCounts: Map[ChronoUnit, Int]

    def toTimeMLValue: String = {
      val counts = this.toUnitCounts
      val dateParts = this.toParts(counts, this.dateChars)
      val timeParts = this.toParts(counts, this.timeChars)
      val timeString = if (timeParts.isEmpty) "" else "T" + timeParts.mkString
      "P" + dateParts.mkString + timeString
    }
    
    private def toParts(counts: Map[ChronoUnit, Int], unitChars: Seq[(ChronoUnit, String)]) = {
      val partOptions = for ((unit, char) <- unitChars) yield counts.get(unit).map(_ + char)
      partOptions.flatten
    }
    

    private val dateChars = Seq(
      ChronoUnit.YEARS -> "Y",
      ChronoUnit.MONTHS -> "M",
      ChronoUnit.WEEKS -> "W",
      ChronoUnit.DAYS -> "D")
    
    private val timeChars = Seq(
      ChronoUnit.HOURS -> "H",
      ChronoUnit.MINUTES -> "M",
      ChronoUnit.SECONDS -> "S")
  }

  object Period {

    def fromParse(tree: Tree): Period = tree match {
      case unit: Tree.Terminal =>
        SimplePeriod(1, Unit.fromParse(unit).value)
      case tree: Tree.NonTerminal => tree.rule.basicSymbol match {
        case "[Period]" => tree.children match {
          case unit :: Nil =>
            SimplePeriod(1, Unit.fromParse(unit).value)
          case amount :: unit :: Nil =>
            SimplePeriod(Number.fromParse(amount).value, Unit.fromParse(unit).value)
          case Tree.Terminal("Sum") :: period1 :: period2 :: Nil =>
            Plus(Period.fromParse(period1), Period.fromParse(period2))
          case _ =>
            fail("Period", tree)
        }
        case _ =>
          fail("Period", tree)
      }
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

  sealed abstract class Mod extends Temporal
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