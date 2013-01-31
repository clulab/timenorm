package info.bethard.timenorm

import scala.collection.immutable.{ Seq, IndexedSeq }
import scala.collection.mutable.Buffer

import org.threeten.bp.temporal.ChronoUnit
import org.threeten.bp.temporal.ChronoField

class Parser(grammar: Grammar) extends (Seq[String] => Parser.Parse){
  def apply(sourceTokens: Seq[String]): Parser.Parse = this.parse(sourceTokens)
  
  def parse(sourceTokens: Seq[String]): Parser.Parse = {
    val chart = this.parseChart(sourceTokens)
    chart(sourceTokens.size)(0).completes match {
      case Buffer(parse) => parse
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

  def parseAll(sourceTokens: Seq[String]): Seq[Parser.Parse] = {
    val chart = this.parseChart(sourceTokens)
    chart(sourceTokens.size)(0).completes.toIndexedSeq
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
        for (symbol <- grammar.symbolsForNumber(token.toInt)) { 
          val rule = Grammar.Rule(symbol, IndexedSeq(token), IndexedSeq(token), Map.empty)
          chart(1)(start).completes += Parser.Parse(rule, IndexedSeq.empty)
        }
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
            if (partial.rule.sourceSeq(partial.sourceSeqIndex) == complete.rule.symbol) {
              val sourceSeqIndex = partial.sourceSeqIndex + 1
              val nonTerminalRules = partial.nonTerminalRules :+ complete
              if (partial.rule.sourceSeq.size == sourceSeqIndex) {
                entry.completes += Parser.Parse(partial.rule, nonTerminalRules)
              } else {
                entry.partials += Parser.PartialParse(partial.rule, sourceSeqIndex, nonTerminalRules)
              }
            }
          }
        }
      }

      // create parses for rules that start with any of the currently complete parses
      // NOTE: we have to use a while-loop here because the loop itself may add more completed
      // rules that we then also need to process
      var i = 0
      while (i < entry.completes.size) {
        val complete = entry.completes(i)
        for (rule <- grammar.sourceSeqStartsWith(complete.rule.symbol)) {
          if (rule.sourceSeq.tail.isEmpty) {
            entry.completes += Parser.Parse(rule, IndexedSeq(complete))
          } else {
            entry.partials += Parser.PartialParse(rule, 1, IndexedSeq(complete))
          }
        }
        i += 1
      }
    }
    chart
  }
}

object Parser {

  case class Parse(
      rule: Grammar.Rule,
      nonTerminalRules: IndexedSeq[Parse])

  private[Parser] case class PartialParse(
    rule: Grammar.Rule,
    sourceSeqIndex: Int,
    nonTerminalRules: IndexedSeq[Parse])

  private[Parser] case class ChartEntry(
    completes: Buffer[Parse] = Buffer.empty,
    partials: Buffer[PartialParse] = Buffer.empty)
}
