package org.clulab.timenorm.scfg

import scala.collection.immutable.{IndexedSeq, Seq}

/**
 * A set of root symbols and synchronous rules that define a synchronous grammar.
 * 
 * Non-terminal symbols should be alphanumeric characters enclosed in square brackets, for example,
 * `[Period]`, `[TimeSpan]` or `[Int]`.
 * 
 * A single colon can be used to "subtype" a non-terminal, e.g. `[Int:4Digit]` or `[Unit:Singular]`.
 * In such cases, the symbol without the subtype information is called the "basic symbol", e.g.
 * `[Int]` and `[Unit]` are the basic symbols for the preceding two examples.
 * 
 * A symbol of the form `[Int:X-Y]`, where X and Y are integers, is treated as a numeric range
 * specification (inclusive of both endpoints). [[SynchronousParser]] has special handling of
 * such ranges.
 * 
 * A non-terminal whose basic symbol is `[Nil]` is called a "nil symbol". [[SynchronousParser]] has
 * special handling of such nil symbols. 
 *
 * @constructor Create a new grammar from a set of root symbols and a set of synchronous rules.
 * @param rootSymbols The symbols that are allowed to be the root of a parse.
 * @param rules The synchronous rules. 
 */
class SynchronousGrammar(val rootSymbols: Set[String], val rules: Seq[SynchronousGrammar.Rule]) {

  private val rulePrefixMap = PrefixMultiMap.empty[String, SynchronousGrammar.Rule]
  for (rule <- rules) {
    this.rulePrefixMap += (rule.sourceSeq, rule)
  }
  
  private val numberRegex = "^\\[Int:(\\d*)-(\\d*)\\]$".r
  private val numberRanges: Set[Range.Inclusive] = (
    for {
      rule <- rules
      numberRegex(begin, end) <- rule.symbol +: rule.sourceSeq
    } yield {
      begin.toInt to end.toInt
    }).toSet
  
  /**
   * Gets all non-terminal symbols whose range allows a particular number.
   * 
   * @param number The number whose possible non-terminal symbols are to be found.
   * @return Each non-terminal symbol whose range allows the number. 
   */
  def sourceSymbolsForNumber(number: Int): Set[String] = {
    val symbolsWithRanges =
      for (range <- this.numberRanges; if range.contains(number))
        yield "[Int:%d-%d]".format(range.start, range.end)
    symbolsWithRanges + "[Int]" 
  }

  /**
   * Gets all rules whose source side starts with a token sequence.
   * 
   * @param tokens The sequence of source tokens.
   * @return All rules whose source side starts with the given tokens.
   */
  def sourceSeqStartsWith(tokens: Seq[String]): Set[SynchronousGrammar.Rule] = {
    this.rulePrefixMap.getAllWithPrefix(tokens)
  }

  /**
   * Gets all rules whose source side starts with a token.
   * 
   * @param token The source token.
   * @return All rules whose source side starts with the given token.
   */
  def sourceSeqStartsWith(token: String): Set[SynchronousGrammar.Rule] = {
    this.rulePrefixMap.getAllWithPrefix(Seq(token))
  }
  
  /**
   * Gets all symbols used in the grammar.
   * 
   * This includes both terminal and non-terminal symbols
   * 
   * @return All symbols in the grammar.
   */
  def sourceSymbols(): Set[String] = {
    this.rules.flatMap(_.sourceSeq).toSet
  }
}

object SynchronousGrammar {

  /**
   * Determines whether a token is a terminal or non-terminal.
   * 
   * Currently, tokens must start with "[" and end with "]" to be a non-terminal.
   * 
   * @param token A token from a grammar.
   * @return True if the token is a terminal, false otherwise.
   */
  def isTerminal(token: String): Boolean = !token.matches("^\\[.*\\]$")

  /**
   * Strips any sub-type information from a non-terminal symbol.
   * 
   * For example, `[Int:4Digit]` would be converted to `[Int]`
   * 
   * @param token A non-terminal token from the grammar.
   * @return A non-terminal token without the sub-type information.
   */
  def basicSymbol(token: String): String = token.replaceAll(":[^\\]]*", "")

  /**
   * Determines whether a token is a number or not.
   * 
   * Currently, only tokens that are all digits are considered to be numbers.
   * 
   * @param token A token from a grammar.
   * @return True if the token is a number, false otherwise.
   */
  def isNumber(token: String): Boolean = token.matches("^\\d+$")
  
  /**
   * Determines whether a token is a nil non-terminal symbol or not.
   * 
   * Currently, only non-terminals whose basic symbol is "[Nil]" are considered to be nils.
   * 
   * @param token A token from a grammar.
   * @return True if the token is a nil non-terminal, false otherwise.
   */
  def isNil(token: String): Boolean = this.basicSymbol(token) == "[Nil]"
  
  /**
   * Parses a [[SynchronousGrammar]] from a string representation.
   * 
   * The first line defines the one or more root symbols and looks like:
   * <pre>
   * ROOTS [Period] [TimeSpan] ...
   * </pre>
   * The remaining lines follow the format of Joshua/Heiro
   * (http://joshua-decoder.org/4.0/file-formats.html) and look like:
   * <pre>
   * [Period] ||| [Period,1] and [Period,2] ||| Sum [Period,1] [Period,2] ||| 1.0
   * ...
   * </pre>
   * 
   * @param text The formatted grammar string.
   * @return A new [[SynchronousGrammar]].
   */
  def fromString(text: String): SynchronousGrammar = {
    val stripLabel: (String => String) = _.replaceAll("\\[(.*),.*\\]", "[$1]")
    // workaround instead of just `text.lines` because jdk11 also defines `String.lines`
    val lines = text.linesWithSeparators.map(_.stripLineEnd)
    val firstLine = lines.next
    if (!firstLine.startsWith("ROOTS")) {
      throw new IllegalArgumentException("First line must define root symbols, e.g. ROOTS [XXX] [YYY]")
    }
    val rootSymbols = firstLine.split("\\s+").tail.toSet
    val nonCommentLines = lines.filterNot(_.startsWith("//"))
    val rules = for (line <- nonCommentLines) yield line.trim.split("\\s*[|][|][|]\\s*") match {
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

  /**
   * A synchronous grammar rule.
   * 
   * @constructor Creates a synchronous grammar rule.
   * @param symbol A non-terminal symbol
   * @param sourceSeq The sequence of source tokens
   * @param targetSeq The sequence of target tokens
   * @param nonTerminalAlignment A mapping from source non-terminal indexes to the corresponding
   *        target non-terminal indexes. For example, <code>Map(1->2, 2->1)</code> would indicate
   *        that the first source non-terminal is the second target non-terminal and vice versa. 
   */
  case class Rule(symbol: String, sourceSeq: IndexedSeq[String], targetSeq: IndexedSeq[String], nonTerminalAlignment: Map[Int, Int]) {
    
    /**
     * Strips any sub-type information from this rule's symbol.
     * 
     * See [[SynchronousGrammar.basicSymbol]].
     */
    val basicSymbol = SynchronousGrammar.basicSymbol(this.symbol)
    
    /**
     * Determines whether this rule's symbol is a nil non-terminal symbol or not.
     *
     * See [[SynchronousGrammar.isNil]].
     */
    val isNil = SynchronousGrammar.isNil(this.symbol)
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

  def getAllWithPrefix(key: Seq[K]): Set[V] = {
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
