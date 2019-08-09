package org.clulab.timenorm.scate

import java.io.InputStream
import java.nio.charset.Charset

import org.apache.commons.io.IOUtils
import org.clulab.timenorm.scfg.SynchronousParser.Tree
import org.clulab.timenorm.scfg.{SynchronousGrammar, SynchronousParser}

import scala.util.{Failure, Success}


object WordsToNumber {
  def apply(languageCode: String): WordsToNumber = languageCode match {
    case "en" => new WordsToNumber(getClass.getResourceAsStream("/org/clulab/timenorm/en.numbers.grammar"))
  }
}

class WordsToNumber(grammarStream: InputStream) extends Function[Array[String], Option[Long]] {

  private val parser = new SynchronousParser(SynchronousGrammar.fromString(
    IOUtils.toString(grammarStream, Charset.forName("ascii"))))

  override def apply(words: Array[String]): Option[Long] = {
    parser.tryParseAll(words.toIndexedSeq) match {
      case Failure(_) => None
      case Success(IndexedSeq(tree)) => Some(this.toDigits(tree).foldLeft(0L) { case (sum, digit) => 10L * sum + digit })
      case Success(trees) => throw new UnsupportedOperationException(
        s"Ambiguous grammar for ${words.toList}. Parses:\n${trees.mkString("\n")}")
    }
  }

  private def toDigits(tree: Tree): List[Long] = tree match {
    case Tree.Terminal(number) => number.toLong :: Nil
    case Tree.NonTerminal(_, children) => children.flatMap(this.toDigits)
  }
}
