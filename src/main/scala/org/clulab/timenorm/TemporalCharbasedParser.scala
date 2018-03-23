package org.clulab.timenorm

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.conf.ComputationGraphConfiguration
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork

import java.util.Arrays
import scala.collection.immutable.IndexedSeq
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import java.time.DateTimeException
import java.time.LocalDate
import java.time.temporal.IsoFields.QUARTER_YEARS
import java.io.FileInputStream

import play.api.libs.json._

object TemporalCharbasedParser {
  private val log: Logger = LoggerFactory.getLogger(TemporalCharbasedParser.getClass)

  def main(args: Array[String]): Unit = {

    val parser = args match {
      case Array(modelFile) =>
        new TemporalCharbasedParser(modelFile)
      case _ =>
        System.err.printf("usage: %s [model-file]\n", this.getClass.getSimpleName)
        System.exit(1)
        throw new IllegalArgumentException
    }


    // use the current date as an anchor
    val now = LocalDate.now()
    val anchor = TimeSpan.of(now.getYear, now.getMonthValue, now.getDayOfMonth)
    System.out.printf("Assuming anchor: %s\n", anchor.timeMLValue)
    System.out.println("Type in a time expression (or :quit to exit)")

    // repeatedly prompt for a time expression and then try to parse it
    System.out.print(">>> ")
    for (line <- Source.stdin.getLines.takeWhile(_ != ":quit")) {
      parser.parse(line, anchor) // match {
      //   case Failure(exception) =>
      //     System.out.printf("Error: %s\n", exception.getMessage)
      //   case Success(temporal) =>
      //     System.out.println(temporal.timeMLValue)
      // }
      System.out.print(">>> ")
    }
  }
}


class TemporalCharbasedParser(modelFile: String) {
  private val network: ComputationGraph = KerasModelImport.importKerasModelAndWeights(modelFile, false)
  private val char2int = readDict(new FileInputStream("/home/egoitz/Tools/time/timenorm/src/main/resources/org/clulab/timenorm/vocab/char2int.txt"))
  private val unicode2int = readDict(new FileInputStream("/home/egoitz/Tools/time/timenorm/src/main/resources/org/clulab/timenorm/vocab/unicate2int.txt"))

  private val unicodes = Array("Cn", "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Me", "Mc", "Nd", "Nl", "No", "Zs", "Zl", "Zp", "Cc", "Cf", "Co", "Cs", "Pd", "Pi", "Pf", "Pc", "Po", "Sm", "Sc", "Sk", "So", "Ps", "Pe")

// UNASSIGNED = 0 [Cn]	Other, Not Assigned (no characters in the file have this property)
// UPPERCASE_LETTER = 1 [Lu]	Letter, Uppercase
// LOWERCASE_LETTER = 2 [Ll]	Letter, Lowercase
// TITLECASE_LETTER = 3 [Lt]	Letter, Titlecase
// MODIFIER_LETTER = 4 [Lm]	Letter, Modifier
// OTHER_LETTER = 5 [Lo]	Letter, Other
// NON_SPACING_MARK = 6 [Mn]	Mark, Nonspacing
// ENCLOSING_MARK = 7 [Me]	Mark, Enclosing
// COMBINING_SPACING_MARK = 8 [Mc]	Mark, Spacing Combining
// DECIMAL_DIGIT_NUMBER = 9 [Nd]	Number, Decimal Digit
// LETTER_NUMBER = 10 [Nl]	Number, Letter
// OTHER_NUMBER = 11 [No]	Number, Other
// SPACE_SEPARATOR = 12 [Zs]	Separator, Space
// LINE_SEPARATOR = 13 [Zl]	Separator, Line
// PARAGRAPH_SEPARATOR = 14 [Zp]	Separator, Paragraph
// CONTROL = 15 [Cc]	Other, Control
// FORMAT = 16 [Cf]	Other, Format
// PRIVATE_USE = 17 [Co]	Other, Private Use
// SURROGATE = 18 [Cs]	Other, Surrogate
// DASH_PUNCTUATION = 19 [Pd]	Punctuation, Dash
// INITIAL_PUNCTUATION = 20 [Pi]	Punctuation, Initial quote (may behave like Ps or Pe depending on usage)
// FINAL_PUNCTUATION = 21 [Pf]	Punctuation, Final quote (may behave like Ps or Pe depending on usage)
// CONNECTOR_PUNCTUATION = 22 [Pc]	Punctuation, Connector
// OTHER_PUNCTUATION = 23 [Po]	Punctuation, Other
// MATH_SYMBOL = 24 [Sm]	Symbol, Math
// CURRENCY_SYMBOL = 25 [Sc]	Symbol, Currency
// MODIFIER_SYMBOL = 26 [Sk]	Symbol, Modifier
// OTHER_SYMBOL = 27 [So]	Symbol, Other
// START_PUNCTUATION = 28 [Ps]	Punctuation, Open
// END_PUNCTUATION = 29 [Pe]	Punctuation, Close


  private def readDict(dictFile: FileInputStream): Map[String, Double] = {
    try {  Json.parse(dictFile).as[Map[String, Double]] } finally { dictFile.close() }
  }


  def parse(sourceText: String, anchor: TimeSpan) { //: Try[Temporal] = {

    val input0 = Nd4j.create(sourceText.map(c => this.char2int.getOrElse(c.toString(), this.char2int("unknown"))).toArray).transpose()
    val input1 = Nd4j.create(sourceText.map(c => this.unicode2int.getOrElse(unicodes(c.getType), this.unicode2int("unknown"))).toArray).transpose()

    this.network.setInput(0, input0)
    this.network.setInput(1, input1)
    this.network.init()
    val results = this.network.feedForward()
    //val nonOperators = results.get("timedistributed_1").map(p => p.max()).toArray
    val nonOperators = (for(p <- 0 to results.get("timedistributed_1").length()) yield results.get("timedistributed_1").getRow(p).max()).toArray
    println(nonOperators.toString)
    val expOperators = (for(p <- 0 to results.get("timedistributed_2").length()) yield results.get("timedistributed_2").getRow(p).max()).toArray
    println(expOperators.toString)
    val impOperators = (for(p <- 0 to results.get("timedistributed_3").length()) yield results.get("timedistributed_3").getRow(p).max()).toArray
    println(impOperators.toString)
  }

  def printModel(){
    println(this.network.summary())
  }
}
