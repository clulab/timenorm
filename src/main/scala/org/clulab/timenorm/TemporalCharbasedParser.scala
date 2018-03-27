package org.clulab.timenorm

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.conf.ComputationGraphConfiguration
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.conf.inputs.InputType

import java.util.Arrays
import scala.collection.immutable.IndexedSeq
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import java.time.DateTimeException
import java.time.LocalDate
import java.time.temporal.IsoFields.QUARTER_YEARS
import java.io.{InputStream, FileInputStream}

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
      //parser.parse("\n\n\n" + line + "\n\n\n", anchor) // match {
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
  private val char2int = readDict(this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/char2int.txt"))
  private val unicode2int = readDict(this.getClass.getResourceAsStream("/org/clulab/timenorm/vocab/unicate2int.txt"))
  // private val operatorLabels = Source.fromResource("/org/clulab/timenorm/label/operator.txt").getLines.toList
  // private val nonOperatorLabels = Source.fromResource("/org/clulab/timenorm/label/non-operator.txt").getLines.toList
  private val operatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/operator.txt")).getLines.toList
  private val nonOperatorLabels = Source.fromInputStream(this.getClass.getResourceAsStream("/org/clulab/timenorm/label/non-operator.txt")).getLines.toList

  //private val unicodes = Array("Cn", "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Me", "Mc", "Nd", "Nl", "No", "Zs", "Zl", "Zp", "Cc", "Cf", "Co", "Cs", "Pd", "Pi", "Pf", "Pc", "Po", "Sm", "Sc", "Sk", "So", "Ps", "Pe")
  private val unicodes = Array("Cn", "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Me", "Mc", "Nd", "Nl", "No", "Zs", "Zl", "Zp", "Cc", "Cf", "Cn", "Co", "Cs", "Pd", "Ps", "Pe", "Pc", "Po", "Sm", "Sc", "Sk", "So", "Pi", "Pf")


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


  private def readDict(dictFile: InputStream): Map[String, Double] = {
    try {  Json.parse(dictFile).as[Map[String, Double]] } finally { dictFile.close() }
  }


  def parse(sourceText: String, anchor: TimeSpan) { //: Try[Temporal] = {

    val d = "\n\n\n2018-10-10\n\n\n"
    val input0 = Nd4j.create(d.map(c => this.char2int.getOrElse(c.toString(), this.char2int("unknown"))).toArray, Array(1,1,16))
    val input1 = Nd4j.create(d.map(c => this.unicode2int.getOrElse(unicodes(c.getType), this.unicode2int("unknown"))).toArray, Array(1,1,16))

    println(input0)
    println(input0.shape().toList)
    this.network.setInput(0, input0)
    this.network.setInput(1, input1)
    //this.network.init()
    val results = this.network.feedForward()

    this.printModel()
    print(results.get(sourceText))
    println(results.get(sourceText).shape().toList)
    //for(r <- results.get(sourceText).toFloatMatrix()) println(r.toList)
    //println(results.get("timedistributed_1"))
    //println(results.get("timedistributed_1").getColumn(0).max())

    val nonOperators = (for (r <- results.get("timedistributed_1").toFloatMatrix()) yield r.indexWhere(x => (x == r.max))).toList.map(o => Try(nonOperatorLabels(o-1)).getOrElse("O"))
    println(nonOperators.toString)
    val expOperators = (for (r <- results.get("timedistributed_2").toFloatMatrix()) yield r.indexWhere(x => (x == r.max))).toList.map(o => Try(operatorLabels(o-1)).getOrElse("O"))
    println(expOperators.toString)
    val impOperators = (for (r <- results.get("timedistributed_3").toFloatMatrix()) yield r.indexWhere(x => (x == r.max))).toList.map(o => Try(operatorLabels(o-1)).getOrElse("O"))
    println(impOperators.toString)
  }

  def printModel(){
    println(this.network.summary())
  }
}
