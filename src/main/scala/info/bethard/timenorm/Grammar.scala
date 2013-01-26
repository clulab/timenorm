package info.bethard.timenorm

import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import scala.Option.option2Iterable
import scala.collection.TraversableOnce.flattenTraversableOnce

// TODO: make this efficient, probably using a trie
class Grammar(val rules: Seq[Grammar.Rule]) {
  def sourceSeqStartsWith(tokens: Seq[String]) = this.rules.filter(_.sourceSeq.startsWith(tokens))
  def sourceSeqStartsWithTerminals(tokens: Seq[String]) = this.rules.filter { rule =>
    rule.sourceSeq.startsWith(tokens) && rule.sourceSeq.take(tokens.size).forall(Grammar.isTerminal)
  }
  def sourceSeqStartsWith(token: String) = this.rules.filter { rule =>
    rule.sourceSeq.nonEmpty && rule.sourceSeq.head == token
  }
}

object Grammar {
  
  val isTerminal: (String => Boolean) = !_.matches("^\\[.*\\]$")
  val isNumber: (String => Boolean) = _.matches("^\\d+$")

  def apply(rules: Seq[Rule]): Grammar = new Grammar(rules)
  
  def fromString(text: String): Grammar = {
    // example:
    // [Period] ||| [Period,1] and [Period,2] ||| Sum [Period,1] [Period,2] ||| 1.0
    val stripLabel: (String => String) = _.replaceAll("\\[(.*),.*\\]", "[$1]")
    val rules = for (line <- text.lines) yield line.trim.split("\\s*[|][|][|]\\s*") match {
      case Array(symbol, sourceSeqString, targetSeqString, scoreString) => { 
        val sourceSeqItems = sourceSeqString.split("\\s+")
        val targetSeqItems = targetSeqString.split("\\s+")
        val sourceNonTerminals = sourceSeqItems.filterNot(this.isTerminal)
        val targetNonTerminals = targetSeqItems.filterNot(this.isTerminal)
        val alignment = for ((token, targetIndex) <- targetNonTerminals.zipWithIndex) yield {
          if (sourceNonTerminals.count(_ == token) != 1) {
            val message = "Expected exactly 1 non-terminal matching \"%s\" in \"%s\""
            throw new IllegalArgumentException(message.format(token, sourceSeqString))
          }
          targetIndex -> sourceNonTerminals.indexOf(token)
        }
        Some(Rule(symbol, sourceSeqItems.map(stripLabel), targetSeqItems.map(stripLabel), alignment.toMap))
      }
      case Array("") => None
      case _ => throw new IllegalArgumentException("\"" + line + "\"")
    }
    Grammar(rules.flatten.toList)
  }

  case class Rule(symbol: String, sourceSeq: Seq[String], targetSeq: Seq[String], nonTerminalAlignment: Map[Int, Int])
}