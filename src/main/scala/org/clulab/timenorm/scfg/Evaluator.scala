import org.clulab.timenorm.scfg._
import scala.util.{Success, Failure}, scala.io.Source
import java.io._
import scala.collection.mutable.ListBuffer

object Evaluator {
  /**
  This program normalizes timexes and compares the results to their gold
  standard normalizations
  */

  def main(lang:String, in_file:String, out_file:String) {
    /**
    Enter the language ("es"/"en") and the input and output paths

    Both input and output files are .tsv files with timexes in the 1st column,
    gold normalization value in the 2nd column and, in the output file, system
    normalization value in the 3rd column. Timexes from different documents must
    be separated by newlines, being DCTs the first timexes from each document
    */

    // Obtain the data of timexes and gold values from the input file
    val (timex_list, gold_list) = get_content(in_file)
    // Obtain the normalizations of the timexes.
    val norm_list = get_normalizations(lang, timex_list)
    // Compare gold and system normalizations, write the results and get the
    //sums of timexes and correct normalizations
    val (sum_gold, sum_norm) = compare_and_write(out_file, timex_list, gold_list, norm_list)

    // Compute number of errors and accuracy
    var sum_errors = sum_gold - sum_norm
    var accuracy = sum_norm.toFloat * 100 / sum_gold

    // Print the final statistics
    println(f"""\n
                |Number of timexes (also DCTs): $sum_gold%6d
                |Correct normalizations:        $sum_norm%6d
                |Incorrect normalizations:      $sum_errors%6d
                |Accuracy:                      $accuracy%6.2f\n""".stripMargin)
  }


  def get_content(in_file:String) : (List[String], List[String]) = {
    /**Obtains the content from the input file as timex and value lists*/

    // Turn input file to a list of lines
    val content = Source.fromFile(in_file).getLines.toList
    // Obtain the standard line length from the first DCT
    val std_line_length = content(0).split("\t").length

    val timex_list = ListBuffer[String]()
    val gold_list = ListBuffer[String]()

    for (line <- content) {
      // If line is not a doc separator (indicated by empty string):
      if (line != "") {
        // If line length equals the standard, get the timex and its gold value
        if (line.split("\t").length == std_line_length) {
          timex_list += line.split("\t").head
          gold_list += line.split("\t").last
        }
        // If this is a detected timex absent in the evaluation corpus, get the
        // timex but append "" as gold normalization
        else {
          timex_list += line.split("\t").head
          gold_list += "-"
        }
      }
      // Otherwise, add empty strings to mark end of document timexes
      else {
        timex_list += ""
        gold_list += ""
      }
    }
    return (timex_list.toList, gold_list.toList)
  }


  def get_normalizations(lang:String, timex_list:List[String]) : List[String] = {
    /**Processes the data, sends timexes and DCTs to the normalizer and returns
    the list with all the normalizations*/

    // Select the parser for the desired grammar depending on the language
    val parser = lang match {
      case "es" => TemporalExpressionParser.es
      case "en" => TemporalExpressionParser.en
      case "it" => TemporalExpressionParser.it
    }

    val norm_list = ListBuffer[String]()
    var dct_timex = ""

    for (timex <- timex_list) {
      // If this is a timex (is not a doc separator):
      if (timex != "") {
        println(timex)
        // If this is the first timex in a doc, consider it a DCT
        if (dct_timex == "") {
          dct_timex = timex
        }
        // Normalize the timex and append the normalization
        var value = normalize(parser, timex, dct_timex)
        norm_list += value
      }
      // If this is a doc separator, empty the DCT timex and append ""
      else {
        dct_timex = ""
        norm_list += ""
      }
    }
    return norm_list.toList
  }


  def normalize(parser:TemporalExpressionParser, timex:String, dct_timex:String) : String = {
    /**Normalizes a timex according to the parser and the DCT timex.
    DCTs are normalized with respect to themselves*/

    // Process the DCT depending on the presence of a time reference
    val pattern = "T".r
    val anchor = pattern.findFirstIn(dct_timex) match {
      // Get the anchor timespan if time is specified
      case Some(_) =>
        val dct = dct_timex.split("T")
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

      // Get the anchor timespan if time is not specified
      case None =>
        val dct = dct_timex.split("-")
        val year = dct(0).toInt
        val month = dct(1).toInt
        val day = dct(2).toInt
        TimeSpan.of(year, month, day)
    }

    // Parse the timex with respect to its anchor
    parser.parse(timex, anchor) match {
      // If the parser fails, return an empty string as normalization
      case Failure(temporal) =>
        return "-"
      // If the parser successes, return the normalization of the timex
      case Success(temporal) =>
        return temporal.timeMLValue
    }
  }


  def compare_and_write(out_file:String, timex_list:List[String],
    gold_list:List[String], norm_list:List[String]) : (Int, Int) = {
    /**Writes the results on the output file, in "{timex}\t{gold}\t{norm}"
    format, and counts the number of timexes and correct normalizations*/

    // Create the output writer
    val writer = new PrintWriter(new File(out_file))

    var gold_counter = 0
    var norm_counter = 0

    // Iterate over timex list and get each timex, gold and norm set
    for (i <- 0 to timex_list.length - 1) {
      val timex = timex_list(i)
      val gold = gold_list(i)
      val norm = norm_list(i)
      println(s"${timex}\t${gold}\t${norm}")

      // If this is a timex, write the data and sum a gold value
      if (timex != "") {
        writer.write(s"${timex}\t${gold}\t${norm}\n")
        gold_counter += 1
        // If timex exists in corpus and normalization is equal to gold,
        // sum a correct norm value
        if (gold != "-" && norm == gold) {norm_counter += 1}
      }
      // If this is a doc separator, write a newline
      else {writer.write(s"\n")}
    }
    writer.close()
    return (gold_counter, norm_counter)
  }


}
