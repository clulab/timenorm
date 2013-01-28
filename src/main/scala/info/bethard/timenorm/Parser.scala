package info.bethard.timenorm

import scala.collection.immutable.{ Seq, IndexedSeq }
import scala.collection.mutable.Buffer

import org.threeten.bp.temporal.ChronoUnit

class Parser(grammar: Grammar) {
  def apply(sourceTokens: Seq[String]): Temporal = {
    val chart = this.parseChart(sourceTokens)
    chart(sourceTokens.size)(0).completes.map(_.toTemporal) match {
      case Buffer(temporal) => temporal
      case Buffer() => {
        val nTokens = sourceTokens.size
        throw new UnsupportedOperationException("Could not parse %s. Partial parses: %s".format(
          sourceTokens.toString,
          for (size <- 1 to nTokens; start <- 0 until (nTokens - size + 1); complete <- chart(size)(start).completes) yield {
            "%s(%s)".format(complete.rule.symbol, sourceTokens.slice(start, start + size).mkString(","))
          }))
      }
      case parses => throw new UnsupportedOperationException("Found multiple parses: " + parses)
    }
  }

  def parseAll(sourceTokens: Seq[String]): Seq[Temporal] = {
    val chart = this.parseChart(sourceTokens)
    chart(sourceTokens.size)(0).completes.map(_.toTemporal).toIndexedSeq
  }

  private def parseChart(sourceTokens: Seq[String]): Array[Array[Parser.ChartEntry]] = {
    val nTokens = sourceTokens.size
    val chart = Array.tabulate(nTokens + 1, nTokens) {
      (size, start) => if (size == 0 || start + size > nTokens) null else Parser.ChartEntry()
    }

    // special handling of [Number]: pass through tokens that are numbers 
    for (start <- 0 until nTokens) {
      val token = sourceTokens(start)
      if (Grammar.isNumber(token)) {
        val rule = Grammar.Rule("[Number]", IndexedSeq(token), IndexedSeq(token), Map.empty)
        chart(1)(start).completes += Parser.Parse(rule, IndexedSeq.empty)
      }
    }

    // fill rules that start with terminals
    for (size <- 1 to nTokens; start <- 0 until (nTokens - size + 1)) {
      val entry = chart(size)(start)
      for (rule <- grammar.sourceSeqStartsWith(sourceTokens.slice(start, start + size))) {
        if (rule.sourceSeq.size == size) {
          entry.completes += Parser.Parse(rule, IndexedSeq.empty)
        } else {
          entry.partials += Parser.PartialParse(rule, size, IndexedSeq.empty)
        }
      }
    }

    // fill in the chart from the smallest sizes to the biggest sizes
    for (size <- 1 to nTokens; start <- 0 to (nTokens - size)) {
      val entry = chart(size)(start)

      // look for ways to create entries of size `size` from the current partial parses
      for (size1 <- 1 until size) {
        val start2 = start + size1
        val size2 = size - size1
        for (partial <- chart(size1)(start).partials) {

          // partials that can be advanced to `size` using terminals
          val newSourceSeqIndex = partial.sourceSeqIndex + size2
          val symbolSeq = partial.rule.sourceSeq.slice(partial.sourceSeqIndex, newSourceSeqIndex)
          val tokenSeq = sourceTokens.slice(start2, start2 + size2)
          if (symbolSeq.forall(Grammar.isTerminal) && symbolSeq == tokenSeq) {
            if (partial.rule.sourceSeq.size == newSourceSeqIndex) {
              entry.completes += Parser.Parse(partial.rule, partial.nonTerminalRules)
            } else {
              entry.partials += Parser.PartialParse(partial.rule, newSourceSeqIndex, partial.nonTerminalRules)
            }
          }

          // partials that can be advanced to `size` using completed non-terminals
          for (complete <- chart(size2)(start2).completes) {
            val completeSeq = partial.rule.sourceSeq.take(partial.sourceSeqIndex) :+ complete.rule.symbol
            for (rule <- grammar.sourceSeqStartsWith(completeSeq)) {
              val nonTerminalRules = partial.nonTerminalRules :+ complete
              if (rule.sourceSeq.size == completeSeq.size) {
                entry.completes += Parser.Parse(rule, nonTerminalRules)
              } else {
                entry.partials += Parser.PartialParse(rule, partial.sourceSeqIndex + 1, nonTerminalRules)
              }
            }
          }
        }
      }

      // create parses for rules that start with any of the currently complete parses
      for (complete <- entry.completes) {
        for (rule <- grammar.sourceSeqStartsWith(complete.rule.symbol)) {
          if (rule.sourceSeq.tail.isEmpty) {
            entry.completes += Parser.Parse(rule, IndexedSeq(complete))
          } else {
            entry.partials += Parser.PartialParse(rule, 1, IndexedSeq(complete))
          }
        }
      }
    }
    chart
  }
}

object Parser {

  private[Parser] case class Parse(
      rule: Grammar.Rule,
      nonTerminalRules: IndexedSeq[Parse]) {

    def toTemporal: Temporal = {
      var nonTerminalIndex = -1
      val targetSeq = for ((token, i) <- this.rule.targetSeq.zipWithIndex) yield {
        if (Grammar.isTerminal(token)) {
          token
        } else {
          nonTerminalIndex += 1
          val nonTerminalRulesIndex = this.rule.nonTerminalAlignment(nonTerminalIndex)
          this.nonTerminalRules(nonTerminalRulesIndex).toTemporal
        }
      }
      val targetList = this.toTemporals(targetSeq.toList)
      this.rule.symbol match {
        case "[Number]" => targetList match {
          case (number: Temporal.Number) :: Nil =>
            number
          case (number: String) :: Nil =>
            Temporal.Number(number.toInt)
          case _ => throw new UnsupportedOperationException(
            "Don't know how to parse [Number] from " + targetList)
        }
        case "[Unit]" => targetList match {
          case (unit: Temporal.Unit) :: Nil =>
            unit
          case (unit: String) :: Nil =>
            Temporal.Unit(ChronoUnit.valueOf(unit))
          case _ => throw new UnsupportedOperationException(
            "Don't know how to parse [Unit] from " + targetList)
        }
        case "[Period]" => targetList match {
          case (period: Temporal.Period) :: Nil =>
            period
          case (unit: Temporal.Unit) :: Nil =>
            Temporal.Period.SimplePeriod(1, unit.value)
          case (amount: Temporal.Number) :: (unit: Temporal.Unit) :: Nil =>
            Temporal.Period.SimplePeriod(amount.value, unit.value)
          case "Sum" :: (period1: Temporal.Period) :: (period2: Temporal.Period) :: Nil =>
            Temporal.Period.Plus(period1, period2)
          case _ => throw new UnsupportedOperationException(
            "Don't know how to parse [Period] from " + targetList)
        }
        case "[Anchor]" => targetList match {
          case (anchor: Temporal.Anchor) :: Nil =>
            anchor
          case "Plus" :: (anchor: Temporal.Anchor) :: (period: Temporal.Period) :: Nil =>
            Temporal.Anchor.Plus(anchor, period)
          case "Minus" :: (anchor: Temporal.Anchor) :: (period: Temporal.Period) :: Nil =>
            Temporal.Anchor.Minus(anchor, period)
          case _ => throw new UnsupportedOperationException(
            "Don't know how to parse [Anchor] from " + targetList)
        }
      }
    }

    private def toTemporals(targetList: List[AnyRef]): List[AnyRef] = {
      targetList match {
        case "TODAY" :: tail =>
          Temporal.Anchor.Today :: toTemporals(tail)
        case "(" :: "Period" :: (amount: String) :: (unit: String) :: ")" :: tail =>
          Temporal.Period.SimplePeriod(amount.toInt, ChronoUnit.valueOf(unit)) :: toTemporals(tail)
        case other :: tail =>
          other :: toTemporals(tail)
        case Nil =>
          Nil
      }
    }
  }

  private[Parser] case class PartialParse(
    rule: Grammar.Rule,
    sourceSeqIndex: Int,
    nonTerminalRules: IndexedSeq[Parse])

  private[Parser] case class ChartEntry(
    completes: Buffer[Parse] = Buffer.empty,
    partials: Buffer[PartialParse] = Buffer.empty)
}
