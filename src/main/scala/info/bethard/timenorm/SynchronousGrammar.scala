package info.bethard.timenorm

import scala.collection.immutable.{ Seq, IndexedSeq }

class SynchronousGrammar(val rootSymbols: Set[String], val rules: Seq[SynchronousGrammar.Rule]) {

  private val rulePrefixMap = PrefixMultiMap.empty[String, SynchronousGrammar.Rule]
  for (rule <- rules) {
    this.rulePrefixMap += (rule.sourceSeq, rule)
  }
  
  private val delexicalizedSymbols = 
    for (rule <- rules; if rule.symbol != rule.basicSymbol)
      yield rule.symbol -> rule.basicSymbol
  for ((symbol, basicSymbol) <- this.delexicalizedSymbols.toMap) {
    val rule = SynchronousGrammar.Rule(basicSymbol, IndexedSeq(symbol), IndexedSeq(symbol), Map(0->0))
    this.rulePrefixMap += (rule.sourceSeq, rule)
  }
  
  private val numberRegex = "^\\[Int:(.*)-(.*)\\]$".r
  private val numberRanges: Set[Range.Inclusive] = (
    for {
      rule <- rules
      numberRegex(begin, end) <- rule.symbol +: rule.sourceSeq
    } yield {
      begin.toInt to end.toInt
    }).toSet
  
  def symbolsForNumber(number: Int): Set[String] = {
    val symbolsWithRanges =
      for (range <- this.numberRanges; if range.contains(number))
        yield "[Int:%d-%d]".format(range.start, range.end)
    symbolsWithRanges + "[Int]" 
  }

  def sourceSeqStartsWith(tokens: Seq[String]) = {
    this.rulePrefixMap.getAllWithPrefix(tokens)
  }

  def sourceSeqStartsWith(token: String) = {
    this.rulePrefixMap.getAllWithPrefix(Seq(token))
  }
}

object SynchronousGrammar {

  val isTerminal: (String => Boolean) = !_.matches("^\\[.*\\]$")
  val isNumber: (String => Boolean) = _.matches("^\\d+$")

  def fromString(text: String): SynchronousGrammar = {
    // example:
    // [Period] ||| [Period,1] and [Period,2] ||| Sum [Period,1] [Period,2] ||| 1.0
    val stripLabel: (String => String) = _.replaceAll("\\[(.*),.*\\]", "[$1]")
    val lines = text.lines
    val firstLine = lines.next
    if (!firstLine.startsWith("ROOTS")) {
      throw new IllegalArgumentException("First line must define root symbols, e.g. ROOTS [XXX] [YYY]")
    }
    val rootSymbols = firstLine.split("\\s+").tail.toSet
    val rules = for (line <- lines) yield line.trim.split("\\s*[|][|][|]\\s*") match {
      case Array(symbol, sourceSeqString, targetSeqString, scoreString) => {
        val sourceSeqItems = sourceSeqString.split("\\s+").toIndexedSeq
        val targetSeqItems = targetSeqString.split("\\s+").toIndexedSeq
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
    new SynchronousGrammar(rootSymbols, rules.flatten.toList)
  }

  case class Rule(symbol: String, sourceSeq: IndexedSeq[String], targetSeq: IndexedSeq[String], nonTerminalAlignment: Map[Int, Int]) {
    val basicSymbol = symbol.replaceAll(":[^\\]]*", "")
    val isNilSymbol = this.basicSymbol == "[Nil]"
  }
}

private[timenorm] class PrefixMultiMap[K, V] {

  var suffixes = Map.empty[K, PrefixMultiMap[K, V]]
  var values = Set.empty[V]

  def +=(key: Seq[K], value: V): Unit = {
    if (key.isEmpty) {
      this.values += value
    } else {
      val head = key.head
      if (!this.suffixes.contains(head)) {
        this.suffixes += head -> new PrefixMultiMap[K, V]
      }
      this.suffixes(head) += (key.tail, value)
    }
  }

  def get(key: Seq[K]): Set[V] = {
    this.getMap(key) match {
      case None => Set.empty
      case Some(map) => map.values
    }
  }

  def getAll: Set[V] = {
    this.values ++ this.suffixes.values.flatMap(_.getAll)
  }

  def getAllWithPrefix(key: Seq[K]) = {
    this.getMap(key) match {
      case None => Set.empty
      case Some(map) => map.getAll
    }
  }

  private def getMap(key: Seq[K]): Option[PrefixMultiMap[K, V]] = {
    if (key.isEmpty) {
      Some(this)
    } else {
      this.suffixes.get(key.head).flatMap(_.getMap(key.tail))
    }
  }
}

private[timenorm] object PrefixMultiMap {
  def empty[K, V] = new PrefixMultiMap[K, V]
}
