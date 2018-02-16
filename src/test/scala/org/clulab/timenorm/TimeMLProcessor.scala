package org.clulab.timenorm

import java.io.File

import scala.collection.JavaConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import java.time.DateTimeException
import org.timen.TIMEN

import com.lexicalscope.jewel.cli.CliFactory
import com.lexicalscope.jewel.cli.{ Option => CliOption }

/**
 * This is not actually a test, but it can be run over TimeML files to see what can and cannot
 * be parsed. To increase coverage, new rules can be added to the timenorm.grammar resource.
 */
object TimeMLProcessor {

  private final val knownFailures = Map[(String, String, String), String](
    ("APW19990122.0193.tml", "t5", "last three years") -> "P1Y" /* "each of the last three years" is not representable in TimeML */,
    ("APW19990206.0090.tml", "t6", "that same day.") -> "1998-10-XX" /* no anchor time given (because anchor is an event) */,
    ("NYT20000414.0296.tml", "t5", "the last week") -> "2000-W15" /* interpreted as "this week" */,
    ("XIE19990210.0079.tml", "t6", "25") -> "1999-04-25" /* "April 24 and 25" */,
    ("AP900815-0044.tml", "t276", "eighth day") -> "1990-08-15" /* "eighth day of Desert Storm" (anchor is an event) */,
    ("APW19980322.0749.tml", "t134", "last week") -> "1998-W12" /* interpreted as "this week" */,
    ("APW19980808.0022.tml", "t4", "10:35 a.m.") -> "1998-08-07T10:35" /* document creation time is whole day, so FindAtOrEarlier finds the one today, not yesterday */,
    ("APW19980213.1320.tml", "t190", "Monday") -> "XXXX-WXX-1TNI" /* "Monday and Tuesday nights" */,
    ("APW19980219.0476.tml", "t137", "weeks or months") -> "PXW" /* requires handling of disjunctions */,
    ("APW19980301.0720.tml", "t1982", "last February") -> "1997-02" /* two Februaries before anchor */,
    ("PRI19980306.2000.1675.tml", "t31", "the second day") -> "1998-03-06" /* "second day of an offensive" (anchor is an event) */,
    ("PRI19980303.2000.2550.tml", "t163", "one day") -> "FUTURE_REF" /* ambiguous with P1D */,
    ("VOA19980305.1800.2603.tml", "t66", "this coming Sunday, March eighth") -> "1998-03-08" /* need handling of ordinals */,
    ("VOA19980305.1800.2603.tml", "t87", "today") -> "PRESENT_REF" /* "today" used like "nowadays" */,
    ("VOA19980331.1700.1533.tml", "t3000", "two") -> "P2D" /* from "two to six days" */,
    ("VOA19980331.1700.1533.tml", "t105", "a year or two") -> "FUTURE_REF" /* requires handling of disjunctions */,
    ("wsj_0144.tml", "t59", "the 1988 period") -> "1988" /* requires handling of corresponding 9 month periods */,
    ("wsj_0171.tml", "t32", "the quarter") -> "1989-Q3" /* interpreted as "the last quarter" */,
    ("wsj_0263.tml", "t2096", "A year earlier") -> "1988-11-01" /* ambiguous with 1988 */,
    ("wsj_0292.tml", "t86", "the 1989 period") -> "1989-Q3" /* "quarter-to-quarter comparison" */,
    ("wsj_0534.tml", "t22", "then") -> "1989-11-30" /* should be handled like PRESENT but not display PRESENT_REF */,
    ("wsj_0570.tml", "t88", "from time to time") -> "FUTURE_REF" /* occurs only once (might be better interpreted as a set instead) */,
    ("wsj_0575.tml", "t213", "that time") -> "P5Y" /* reference to a period */,
    ("wsj_0585.tml", "t1111", "two to three weeks") -> "P2W" /* requires handling of disjunctions */,
    ("wsj_0585.tml", "t386", "the 1988 quarter") -> "1988-Q3" /* requires handling of corresponding quarters */,
    ("wsj_0637.tml", "t46", "the next 12 to 18 months") -> "PXM" /* requires handling of disjunctions */,
    ("wsj_0662.tml", "t36", "the quarter") -> "1989-Q2" /* really means "last quarter" */,
    ("wsj_0709.tml", "t30", "the comparable year-ago quarter") -> "1988-Q2" /* requires handling of corresponding quarters */,
    ("wsj_0760.tml", "t60", "the year-earlier period") -> "P9M" /* reference to a period */,
    ("wsj_0768.tml", "t2004", "recent weeks and months") -> "PAST_REF" /* requires handling of conjunctions of unspecified periods */,
    ("wsj_0810.tml", "t167", "last") -> "1988" /* "...this year as last" */,
    ("wsj_0904.tml", "t247", "the year-earlier nine months") -> "P9M" /* not sure how to capture this */,
    ("wsj_0918.tml", "t200", "the quarter") -> "1989-Q3" /* really means "last quarter" */,
    ("wsj_0928.tml", "t74", "the 1988 period") -> "1988-Q1" /* requires handling of corresponding quarters */,
    ("wsj_1003.tml", "t183", "first two quarters of 1990") -> "1990-H1" /* grammar could be adjusted to handle this, but would normalize it to 1990-01-01 + 2 QUARTERS */,
    ("wsj_1003.tml", "t211", "a year ago") -> "1988-10-26" /* ambiguous with 1988 */,
    ("wsj_1003.tml", "t215", "a year ago") -> "1988-10-26" /* ambiguous with 1988 */,
    ("wsj_1011.tml", "t60", "last spring") -> "1988-SP" /* interpreted as the spring of the last year, but ambiguous with the spring earlier this year */,
    ("wsj_1013.tml", "t167", "a year earlier") -> "1988-QX" /* annotation is correct given anchor, but anchor should not be QX */,
    ("wsj_1014.tml", "t283", "at least the past 18 months") -> "P18M" /* modifier goes on Period but is syntactically attached to TimeSpan */,
    ("wsj_1014.tml", "t371", "the last half of 1989") -> "1989-H2" /* currently no handling of half-TimeSpans */,
    ("wsj_1014.tml", "t446", "the last half of the '80s") -> "198" /* currently no handling of half-TimeSpans */,
    ("wsj_1025.tml", "t40", "second") -> "XXXX-Q2" /* "second and fourth quarters */)

  trait Options {
    @CliOption(longName = Array("corpus-paths"))
    def getCorpusPaths: java.util.List[File]
    @CliOption(longName = Array("fail-on-no-correct-parse"))
    def getFailOnNoCorrectParse: Boolean
  }

  class Stats {
    var total = 0
    var correct = 0
  }

  def main(args: Array[String]): Unit = {
    val options = CliFactory.parseArguments(classOf[Options], args: _*)
    def error(message: String, timex: TimeMLDocument#TimeExpression, file: File) = {
      printf("%s \"%s\" (%s) from %s\n", message, timex.text, timex.value, file)
    }
    def fatal(message: String, timex: TimeMLDocument#TimeExpression, file: File, cause: Throwable) = {
      if (options.getFailOnNoCorrectParse) {
        val exceptionMessage = String.format(
          "%s \"%s\" (%s) from %s\ntimex: %s\nanchor: %s\n(\"%s\", \"%s\", \"%s\", \"%s\")",
          message, timex.text, timex.value, file, timex.elem, timex.anchor.map(_.elem),
          file.getName, timex.id, timex.text, timex.value)
        throw new Exception(exceptionMessage, cause)
      } else {
        error(message, timex, file)
      }
    }

    val normalizers = Seq(new TemporalExpressionParser, new TIMEN)
    val corpusFiles = options.getCorpusPaths.asScala
    def createStatsSeq =
      for (corpusFile <- corpusFiles; normalizer <- normalizers)
        yield (corpusFile, normalizer) -> new Stats
    val fileNormalizerStats = createStatsSeq.toMap

    for {
      corpusFile <- corpusFiles
      file <- this.allFiles(corpusFile)
      doc = new TimeMLDocument(file)
      docCreationTime = TimeSpan.fromTimeMLValue(doc.creationTime.value)
      timex <- doc.timeExpressions
    } {
      // determine if this time expression is a known annotation error or system failure
      val key = (file.getName, timex.id, timex.text)
      val isKnownFailure = this.knownFailures.contains(key)

      // update totals
      for (normalizer <- normalizers) {
        fileNormalizerStats(corpusFile, normalizer).total += 1
      }

      // pick the single best parse and evaluate it
      val anchorOption = timex.anchor.flatMap(timex =>
        if (timex.value.isEmpty ||
          timex.value.startsWith("P") ||
          timex.value.startsWith("T") ||
          timex.value.contains('X')) None
        else Some(TimeSpan.fromTimeMLValue(timex.value)))
      val anchor = anchorOption.getOrElse(docCreationTime)

      // normalize the time expression given the anchor
      val results = for (normalizer <- normalizers) yield {
        val value =
          try {
            normalizer match {
              case n: TemporalExpressionParser =>
                n.parse(timex.text, anchor).map(_.timeMLValue).getOrElse("")
              case n: TIMEN =>
                n.normalize(timex.text, doc.creationTime.value)
            }
          } catch {
            case e: Throwable => ""
          }

        // determine if the normalized value was correct
        val isCorrect = value == timex.value
        if (isCorrect) {
          fileNormalizerStats(corpusFile, normalizer).correct += 1
        }

        // if a known error has been fixed, log it so that it can be removed from the list 
        if (isCorrect && isKnownFailure) {
          normalizer match {
            case n: TemporalExpressionParser => System.err.println("Failure has been fixed: " + key)
            case _ =>
          }
        }

        // if it's incorrect, log the error
        if (!isCorrect) {
          normalizer match {
            case n: TemporalExpressionParser => n.parseAll(timex.text, anchor) match {
              case Failure(e) => fatal("Error parsing", timex, file, e)
              case Success(temporals) => {
                val values = temporals.map(_.timeMLValue)
                if (!values.toSet.contains(timex.value)) {
                  fatal("All incorrect values %s for".format(values), timex, file, null)
                } else {
                  error("Incorrect value chosen from %s for".format(values), timex, file)
                }
              }
            }
            case _ =>
          }
        }
        
        // yield the normalizer results to allow later comparison
        (normalizer, value, isCorrect)
      }
      
      // if the normalizers had different predictions, log the incorrect one
      if (results.map(_._3).toSet == Set(true, false)) {
        val Seq((normalizer, value, correct)) = results.filter(_._3 == false)
        val message = "%s incorrect (%s) for".format(normalizer.getClass.getSimpleName, value)
        error(message, timex, file)
      }
    }

    // print out performance on each corpus
    for (corpusFile <- corpusFiles) {
      printf("======================================================================\n")
      printf("Corpus: %s\n", corpusFile)
      for (normalizer <- normalizers) {
        val stats = fileNormalizerStats(corpusFile, normalizer)
        printf("%-15s\tcorrect: %5d\ttotal: %5d\taccuracy: %.3f\n",
          normalizer.getClass.getSimpleName,
          stats.correct,
          stats.total,
          stats.correct.toDouble / stats.total.toDouble)
      }
    }
    printf("======================================================================\n")
  }

  def allFiles(fileOrDir: File): Iterator[File] = {
    if (fileOrDir.isDirectory) {
      fileOrDir.listFiles.iterator.map(this.allFiles).flatten
    } else {
      Iterator(fileOrDir)
    }
  }
}