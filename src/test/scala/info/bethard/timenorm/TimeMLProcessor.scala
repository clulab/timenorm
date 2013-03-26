package info.bethard.timenorm

import java.io.File

import scala.collection.JavaConverters._

import org.threeten.bp.DateTimeException

import com.lexicalscope.jewel.cli.CliFactory
import com.lexicalscope.jewel.cli.{Option => CliOption}

/**
 * This is not actually a test, but it can be run over TimeML files to see what can and cannot
 * be parsed. To increase coverage, new rules can be added to the timenorm.grammar resource.
 */
object TimeMLProcessor {

  private final val annotationErrors = Set(
    ("APW19980811.0474.tml", "t5", "last week", "1998-08-04"),
    ("APW19980818.0515.tml", "t7", "last week", "P1W"),
    ("APW19980820.1428.tml", "t1", "Friday", "1998-08-07"),
    ("APW19980820.1428.tml", "t2", "this month", "P1M"),
    ("APW19980826.0389.tml", "t4", "September", "P1M"),
    ("APW19981205.0374.tml", "t0", "12/05/1998 09:42:00", "1998-12-05T09:42"),
    ("APW19981205.0374.tml", "t3", "early this week", "1998-12-XX"),
    ("APW19990122.0193.tml", "t0", "1999-01-22 13:06:18", "1999-01-22T13:06"),
    ("APW19990122.0193.tml", "t3", "last year", "1998-10"),
    ("APW19990122.0193.tml", "t5", "last three years", "P1Y"),
    ("APW19990122.0193.tml", "t7", "Last week", "1999-01"),
    ("APW19990122.0193.tml", "t8", "weeks", "1998-10"),
    ("APW19990206.0090.tml", "t0", "1999-02-06 06:22:26", "1999-02-06T06:22"),
    ("APW19990206.0090.tml", "t3", "this week", "1999-02"),
    ("APW19990206.0090.tml", "t6", "that same day.", "1998-10-XX") /* no anchor given */ ,
    ("APW19990216.0198.tml", "t0", "1999-02-16 12:55:33", "1999-02-16T12:55"),
    ("APW19990216.0198.tml", "t4", "50 years ago", "1949-XX"),
    ("APW19990312.0251.tml", "t0", "1999-03-12 10:34:13", "1999-03-12T10:34"),
    ("APW19990312.0251.tml", "t4", "less than a decade", "PXY"),
    ("APW19990312.0251.tml", "t6", "a decade ago", "1989-XX"),
    ("APW19990312.0251.tml", "t7", "1968", "1968-XX"),
    ("APW19990312.0251.tml", "t9", "1949", "1949-XX"),
    ("APW19990312.0251.tml", "t10", "1952", "1952-XX"),
    ("APW19990312.0251.tml", "t11", "1955", "1955-XX"),
    ("APW19990312.0251.tml", "t12", "1982", "1982-XX"),
    ("APW19990312.0251.tml", "t14", "1949", "1949-XX"),
    ("APW19990312.0251.tml", "t17", "A decade ago", "1989-XX"),
    ("APW19990312.0251.tml", "t20", "this century", "P100Y"),
    ("APW19990410.0123.tml", "t0", "1999-04-10 06:27:43", "1999-04-10T06:27"),
    ("APW19990410.0123.tml", "t5", "days", "1998-FA"),
    ("APW19990506.0155.tml", "t0", "1999-05-06 15:05:11", "1999-05-06T15:05"),
    ("APW19990506.0155.tml", "t7", "weeks", "1998-10"),
    ("APW19990507.0207.tml", "t0", "1999-05-07 03:44:53", "1999-05-07T03:44"),
    ("APW19990507.0207.tml", "t10", "weeks", "1998-FA"),
    ("APW19990607.0041.tml", "t0", "1999-06-07 19:00:11", "1999-06-07T19:00"),
    ("APW19991008.0151.tml", "t5", "Thursday morning", "1999-10-07"),
    ("APW19991024.0075.tml", "t0", "1999-10-24 20:00:09", "1999-10-24T20:00"),
    ("APW19991024.0075.tml", "t5", "tonight", "1999-10-24"),
    ("APW19991024.0075.tml", "t8", "Earlier that day", "1998-10-23") /* no anchor given */ ,
    ("APW199980817.1193.tml", "t3", "Tuesday morning", "1998-08-18"),
    ("APW199980817.1193.tml", "t8", "the weekend", "P2D"),
    ("APW199980817.1193.tml", "t10", "last week", "P1W"),
    ("APW20000106.0064.tml", "t1", "Wednesday night", "2000-01-05"),
    ("APW20000107.0088.tml", "t10", "Wednesday night", "2000-01-05TEV"),
    ("APW20000107.0318.tml", "t9", "early next week", "2000-W2") /* format should be Wxx */ ,
    ("APW20000107.0318.tml", "t10", "Friday night", "2000-01-07TEV"),
    ("APW20000107.0318.tml", "t15", "Friday night", "2000-01-07"),
    ("APW20000115.0031.tml", "t7", "this week", "P1W"),
    ("APW20000115.0031.tml", "t10", "decades-long", "PXY"),
    ("APW20000115.0209.tml", "t1", "Thursday", "2000-01-06") /* DCT is wrong */ ,
    ("APW20000115.0209.tml", "t5", "Wednesday", "2000-01-05") /* DCT is wrong */ ,
    ("APW20000115.0209.tml", "t8", "Wednesday", "2000-01-05") /* DCT is wrong */ ,
    ("APW20000115.0209.tml", "t10", "Wednesday night", "2000-01-05TEV"),
    ("APW20000115.0209.tml", "t12", "Wednesday", "2000-01-05") /* DCT is wrong */ ,
    ("APW20000128.0316.tml", "t22", "later that year", "1998") /* no anchor given */ ,
    ("APW20000210.0328.tml", "t2", "last week", "2000-W7"),
    ("APW20000210.0328.tml", "t4", "this week", "2000-W6") /* format should be Wxx */ ,
    ("APW20000216.0193.tml", "t4", "this week", "2000-W7") /* format should be Wxx */ ,
    ("APW20000216.0272.tml", "t6", "this week", "2000-W7") /* format should be Wxx */ ,
    ("APW20000401.0150.tml", "t2", "more than four decades ago", "P40Y"),
    ("APW20000403.0057.tml", "t3", "Tuesday", "2000-04-05"),
    ("APW20000405.0276.tml", "t4", "4 in the morning", "2000-04-06T04:00") /* no anchor given */ ,
    ("APW20000405.0276.tml", "t10", "Wednesday night", "2000-04-05"),
    ("APW20000417.0031.tml", "t3", "Sunday night", "2000-04-16"),
    ("APW20000417.0031.tml", "t6", "Sunday night", "2000-04-16"),
    ("NYT19980907.0112.tml", "t11", "years-old", "P1Y"),
    ("NYT19980907.0112.tml", "t9", "13 years", "P12Y"),
    ("NYT19981025.0188.tml", "t3", "past decade", "P10Y"),
    ("NYT19981025.0188.tml", "t5", "Last week", "1998-10"),
    ("NYT19981025.0188.tml", "t8", "week", "1998-01"),
    ("NYT19981025.0216.tml", "t5", "1992", "1992-05-10"),
    ("NYT19981025.0216.tml", "t7", "1980s", "198X"),
    ("NYT19981025.0216.tml", "t8", "1990s", "199X"),
    ("NYT19981025.0216.tml", "t9", "1992", "1992-10-02"),
    ("NYT19981026.0446.tml", "t6", "last week", "1998-10-XX"),
    ("NYT19981026.0446.tml", "t7", "earlier this year", "1998-XX-XX"),
    ("NYT19981026.0446.tml", "t13", "first day", "1999-01-XX") /* "first day in office" ??? */ ,
    ("NYT19981026.0446.tml", "t14", "next week", "1998-11-03"),
    ("NYT19981120.0362.tml", "t7", "days", "1998-10-XX"),
    ("NYT19981121.0173.tml", "t2", "last week", "1998-11"),
    ("NYT19981121.0173.tml", "t11", "day", "1998-10-24"),
    ("NYT19990312.0271.tml", "t4", "1948", "1948-XX"),
    ("NYT19990312.0271.tml", "t6", "10 years ago", "1989-XX"),
    ("NYT19990419.0515.tml", "t4", "next week.", "1999-04-XX"),
    ("NYT19990419.0515.tml", "t9", "6 a.m. ten days", "1998-10-13T06:00"),
    ("NYT19990419.0515.tml", "t16", "days", "199X-XX-XX"),
    ("NYT19990419.0515.tml", "t14", "1994", "1994-XX-XX"),
    ("NYT19990419.0515.tml", "t11", "1995", "1995-XX-XX"),
    ("NYT19990419.0515.tml", "t12", "1997", "1997-XX-XX"),
    ("NYT19990419.0515.tml", "t15", "10 p.m", "1998-10-23T22:00") /* no anchor given */ ,
    ("NYT19990505.0443.tml", "t8", "days", "1998-10"),
    ("NYT19990505.0443.tml", "t10", "6 a.m", "1998-10-13T06:00") /* no anchor given */ ,
    ("NYT19990505.0443.tml", "t11", "10 days", "1998-10-13"),
    ("NYT20000224.0173.tml", "t3", "this week", "2000-W8") /* format should be Wxx */ ,
    ("NYT20000224.0173.tml", "t5", "1960s", "196X"),
    ("NYT20000224.0173.tml", "t10", "early 1990s", "199X"),
    ("NYT20000224.0173.tml", "t16", "the next morning", "2000-02-23"),
    ("NYT20000329.0359.tml", "t4", "Thursday morning", "2000-03-30"),
    ("NYT20000330.0406.tml", "t1", "Tuesday morning", "2000-04-04"),
    ("NYT20000403.0463.tml", "t20", "9 a.m. EST Tuesday", "2000-04-03T09:00") /* not a Tuesday */ ,
    ("NYT20000403.0463.tml", "t21", "the weekend", "P2D"),
    ("NYT20000424.0319.tml", "t4", "the weekend", "2000-04-22"),
    ("NYT20000601.0442.tml", "t5", "six months", "P6D"),
    ("XIE19980808.0031.tml", "t3", "Friday morning", "1998-08-07"),
    ("XIE19980814.0294.tml", "t0", "August 13", "1998-08-14"),
    ("XIE19990210.0079.tml", "t3", "Wednesday", "1999-02-09") /* not a Wednesday */ ,
    ("XIE19990210.0079.tml", "t6", "25", "1999-04-25") /* "April 24 and 25" ??? */ ,
    ("XIE19990210.0079.tml", "t9", "Tuesday", "1999-02-08") /* not a Tuesday */ ,
    ("XIE19990313.0031.tml", "t3", "day", "1999-03-12") /* "a great day" - no anchor given */ ,
    ("XIE19990313.0229.tml", "t3", "day", "1999-03-12") /* "a great day" - no anchor given */ ,
    ("XIE19990313.0229.tml", "t4", "this century,", "P100Y"),
    ("ABC19980108.1830.0711.tml", "t85", "the last twenty four hours", "P1D"),
    ("AP900815-0044.tml", "t188", "Tuesday", "1990-08-07") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t204", "Tuesday", "1990-08-07") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t210", "Monday", "1990-08-06") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t252", "Tuesday", "1990-08-07") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t269", "Tuesday", "1990-08-07") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900816-0139.tml", "t352", "A day earlier", "P1D") /* wrong type of value and no anchor */)
  
  private final val knownFailures = Set(
    ("AP900816-0139.tml", "t339", "a fairly lengthy period", "PXX"),
    ("APW19980213.1320.tml", "t190", "Monday", "XXXX-WXX-1TNI"))

  trait Options {
    @CliOption(longName=Array("corpus-paths"))
    def getCorpusPaths: java.util.List[File]
    @CliOption(longName=Array("fail-on-no-correct-parse"))
    def getFailOnNoCorrectParse: Boolean
  }

  def main(args: Array[String]): Unit = {
    val options = CliFactory.parseArguments(classOf[Options], args: _*)
    def error(message: String, args: Any*) = {
      println(message.format(args: _*))
    }
    def fatal(message: String, args: Any*) = {
      if (options.getFailOnNoCorrectParse) {
        throw new Exception(message.format(args: _*))
      } else {
        println(message.format(args: _*))
      }
    }
    
    val normalizer = new TimeNormalizer

    val corpusStats = for (corpusFile <- options.getCorpusPaths.asScala) yield {
      val resultsIter: Iterator[Boolean] = for {
        file <- this.allFiles(corpusFile)
        doc = new TimeMLDocument(file)
        timex <- doc.timeExpressions
      } yield {
        val key = (file.getName, timex.id, timex.text, timex.value)
        val isAnnotationError = this.annotationErrors.contains(key)
        val isKnownFailure = this.knownFailures.contains(key)
        val isPossibleFailure = !isAnnotationError && !isKnownFailure

        // pick the single best parse and evaluate it
        val anchor = timex.anchor.getOrElse(doc.creationTime)
        try {
          val parses = normalizer.parseAll(timex.text).toSeq
          val temporal = normalizer.normalize(parses, anchor)
          val value = temporal.map(_.timeMLValue).getOrElse("")
          val isCorrect = value == timex.value

          // if it's incorrect, log the error
          if (!isCorrect) {

            // see if any of the alternative parses had the correct value
            val possibleAnchors = timex.anchor ++ Seq(doc.creationTime)
            val possibleParses = normalizer.parseAll(timex.text).toSeq
            val possibleValues = for (anchor <- possibleAnchors; parse <- possibleParses) yield {
              normalizer.normalize(parse, anchor).timeMLValue
            }

            // log the error
            if (isPossibleFailure && !possibleValues.toSet.contains(timex.value)) {
              fatal("All incorrect values %s for %s from %s", possibleValues, timex, file)
            } else {
              error("Incorrect value %s for %s from %s", value, timex, file)
            }
          }

          // yield whether the prediction was correct or not
          isCorrect

        } catch {
          // on an exception
          case e @ (_: UnsupportedOperationException | _: DateTimeException) => {
            if (isPossibleFailure) {
              fatal("Error \"%s\" parsing %s from %s", e, timex, file)
            }
            false
          }
        }
      }

      // calculate the total time expressions and the number of values that were correct
      val results = resultsIter.toSeq
      val total = results.size
      val correct = results.count(_ == true)
      (corpusFile, total, correct)
    }

    // print out performance on each corpus
    for ((corpusFile, total, correct) <- corpusStats) {
      printf("============================================================\n")
      printf("Corpus: %s\n", corpusFile)
      printf("Accuracy: %.3f\n", correct.toDouble / total.toDouble)
    }
    printf("============================================================\n")
  }

  def allFiles(fileOrDir: File): Iterator[File] = {
    if (fileOrDir.isDirectory) {
      fileOrDir.listFiles.iterator.map(this.allFiles).flatten
    } else {
      Iterator(fileOrDir)
    }
  }
}