package org.clulab.timenorm.scfg

import scala.collection.mutable.ListBuffer
import scala.util.{Success, Failure}, scala.io.Source

import java.io._

object Evaluator {
  /**
  This program normalizes timexes and compares the results to their gold
  standard normalizations
  */

  def main(lang: String, inFile: String, outFile: String): Unit = {
    /**
    Enter the language ("es"/"en") and the input and output paths

    Both input and output files are .tsv files with timexes in the 1st column,
    gold normalization value in the 2nd column and, in the output file, system
    normalization value in the 3rd column. Timexes from different documents must
    be separated by newlines, being DCTs the first timexes from each document
    */

    // Obtain the data of timexes and gold values from the input file
    val (timexList, goldList) = getContent(inFile)
    // Obtain the normalizations of the timexes.
    val normList = getNormalizations(lang, timexList)
    // Compare gold and system normalizations, write the results and get the
    //sums of timexes and correct normalizations
    val (sumGold, sumNorm) = compareAndWrite(outFile, timexList, goldList, normList)

    // Compute number of errors and accuracy
    val sumErrors = sumGold - sumNorm
    val accuracy = sumNorm.toFloat * 100 / sumGold

    // Print the final statistics
    println(f"""\n
                |Number of timexes (also DCTs): $sumGold%6d
                |Correct normalizations:        $sumNorm%6d
                |Incorrect normalizations:      $sumErrors%6d
                |Accuracy:                      $accuracy%6.2f\n""".stripMargin)
  }


  def getContent(inFile: String): (List[String], List[String]) = {
    /** Obtains the content from the input file as timex and value lists */

    // Turn input file to a list of lines
    val content = Source.fromFile(inFile).getLines.toList
    // Obtain the standard line length from the first DCT
    val stdLineLength = content(0).split("\t").length

    val timexList = ListBuffer[String]()
    val goldList = ListBuffer[String]()

    for (line <- content) {
      // If line is not a doc separator (indicated by empty string):
      if (line != "") {
        // If line length equals the standard, get the timex and its gold value
        if (line.split("\t").length == stdLineLength) {
          timexList += line.split("\t").head
          goldList += line.split("\t").last
        }
        // If this is a detected timex absent in the evaluation corpus, get the
        // timex but append "" as gold normalization
        else {
          timexList += line.split("\t").head
          goldList += "-"
        }
      }
      // Otherwise, add empty strings to mark end of document timexes
      else {
        timexList += ""
        goldList += ""
      }
    }
    (timexList.toList, goldList.toList)
  }


  def getNormalizations(lang: String, timexList: List[String]): List[String] = {
    /** Processes the data, sends timexes and DCTs to the normalizer and returns
    the list with all the normalizations */

    // Select the parser for the desired grammar depending on the language
    val parser = lang match {
      case "es" => TemporalExpressionParser.es
      case "en" => TemporalExpressionParser.en
      case "it" => TemporalExpressionParser.it
    }

    val normList = ListBuffer[String]()
    var dctTimex = ""

    for (timex <- timexList) {
      // If this is a timex (is not a doc separator):
      if (timex != "") {
        println(timex)
        // If this is the first timex in a doc, consider it a DCT
        if (dctTimex == "") {
          dctTimex = timex
        }
        // Normalize the timex and append the normalization
        val value = normalize(parser, timex, dctTimex)
        normList += value
      }
      // If this is a doc separator, empty the DCT timex and append ""
      else {
        dctTimex = ""
        normList += ""
      }
    }
    normList.toList
  }


  def normalize(parser: TemporalExpressionParser, timex: String, dctTimex: String): String = {
    /** Normalizes a timex according to the parser and the DCT timex.
    DCTs are normalized with respect to themselves */

    // Process the DCT depending on the presence of a time reference
    val anchor = if (dctTimex.contains('T')) {
      val dct = dctTimex.split("T")
      val date = dct(0).split("-")
      val time = dct(1).split(":")
      val year = date(0).toInt
      val month = date(1).toInt
      val day = date(2).toInt
      val hours = time(0).toInt
      val minutes = time(1).toInt
      val seconds = time.length match {
        case 3 => time(2).toInt
        case _ => 0
      }
      TimeSpan.of(year, month, day, hours, minutes, seconds)
    }
    else {
      // Get the anchor timespan if time is not specified
      val dct = dctTimex.split("-")
      val year = dct(0).toInt
      val month = dct(1).toInt
      val day = dct(2).toInt
      TimeSpan.of(year, month, day)
    }

    // Parse the timex with respect to its anchor
    parser.parse(timex, anchor) match {
      // If the parser fails, return an empty string as normalization
      case Failure(temporal) =>
        "-"
      // If the parser successes, return the normalization of the timex
      case Success(temporal) =>
        temporal.timeMLValue
    }
  }


  def compareAndWrite(outFile: String, timexList: List[String],
      goldList: List[String], normList: List[String]): (Int, Int) = {
    /** Writes the results on the output file, in "{timex}\t{gold}\t{norm}"
    format, and counts the number of timexes and correct normalizations */

    // Create the output writer
    val printWriter = new PrintWriter(new File(outFile))

    var goldCounter = 0
    var normCounter = 0

    // Iterate over timex list and get each timex, gold and norm set
    for (i <- timexList.indices) {
      val timex = timexList(i)
      val gold = goldList(i)
      val norm = normList(i)
      println(s"${timex}\t${gold}\t${norm}")

      // If this is a timex, write the data and sum a gold value
      if (timex != "") {
        printWriter.println(s"${timex}\t${gold}\t${norm}")
        goldCounter += 1
        // If timex exists in corpus and normalization is equal to gold,
        // sum a correct norm value
        if (gold != "-" && norm == gold) {normCounter += 1}
      }
      // If this is a doc separator, write a newline
      else
        printWriter.println
    }
    printWriter.close()
    (goldCounter, normCounter)
  }
}
