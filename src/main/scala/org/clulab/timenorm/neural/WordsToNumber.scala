package org.clulab.timenorm.neural

import java.io.InputStream
import java.nio.charset.Charset

import org.apache.commons.io.IOUtils
import org.clulab.timenorm.scfg.SynchronousParser.Tree
import org.clulab.timenorm.scfg.{SynchronousGrammar, SynchronousParser}


object WordsToNumber {
  def apply(languageCode: String): WordsToNumber = languageCode match {
    case "en" => new WordsToNumber(getClass.getResourceAsStream("/org/clulab/timenorm/en.numbers.grammar"))
  }
}

class WordsToNumber(grammarStream: InputStream) extends Function[Array[String], Long] {

  private val grammar = SynchronousGrammar.fromString(IOUtils.toString(grammarStream, Charset.forName("ascii")))
  private val parser = new SynchronousParser(grammar)

  override def apply(words: Array[String]): Long = parser.parseAll(words) match {
    case Array(tree) => this.toDigits(tree).reverse.zipWithIndex.foldLeft(0L){
      case (sum, (digit, index)) => sum + digit * Array.fill(index)(10L).product
    }
    case trees => throw new UnsupportedOperationException(
        s"Ambiguous grammar for ${words.toList}. Parses:\n${trees.mkString("\n")}")
  }

  private def toDigits(tree: Tree): List[Long] = tree match {
    case Tree.Terminal(number) => number.toLong :: Nil
    case Tree.NonTerminal(_, children) => children.flatMap(this.toDigits)
  }
}
