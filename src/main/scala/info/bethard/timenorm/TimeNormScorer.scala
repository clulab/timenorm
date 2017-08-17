package info.bethard.timenorm

import info.bethard.timenorm.formal.{Interval, ThisRIs, TimeExpression}
import info.bethard.timenorm.formal._
import java.time.temporal.ChronoField._
import java.time.temporal.ChronoUnit._

import info.bethard.anafora.{Annotation, Data, Entity, Properties}
import java.io.{File, FileNotFoundException}
import java.time.format.DateTimeParseException
import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import java.time.temporal.TemporalUnit


object TimeNormScorer {
  
  def get_intervals(timex: TimeExpression): Seq[Interval] = timex match {
    case interval: Interval => List(interval)
    case intervals: Intervals => intervals
    case _ => Seq.empty
  }

  def epoch(datetime: java.time.LocalDateTime): Long = datetime.atZone(ZoneId.systemDefault).toEpochSecond
  def datetime(epoch: Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochSecond(epoch), ZoneId.systemDefault())

  def parseDCT(dctString: String): Interval = dctString.split("T") match {
    case Array(date) => date.split("-").map(_.toInt) match {
      case Array(year) => SimpleInterval.of(year)
      case Array(year, month) => SimpleInterval.of(year, month)
      case Array(year, month, day) => SimpleInterval.of(year, month, day)
    }
    case Array(date, time) => date.split("-").map(_.toInt) match {
      case Array(year, month, day) => time.split(":").map(_.toInt) match {
        case Array(hours) => SimpleInterval.of(year, month, day, hours)
        case Array(hours, minutes) => SimpleInterval.of(year, month, day, hours, minutes)
        case Array(hours, minutes, seconds) => SimpleInterval.of(year, month, day, hours, minutes, seconds)
      }
    }
  }

  def score(timex1: TimeExpression, timex2: TimeExpression): (Double, Double) = {
    // find the earliest interval start and the latest interval end (so RepeatingIntervals can be bounded)
    val intervalUnion: Seq[Interval] = Seq(timex2, timex2).flatMap(get_intervals)
    val window = SimpleInterval(intervalUnion.map(_.start).min, intervalUnion.map(_.end).min)

    // intersect the intervals from each time expression, and take the sum of their sizes
    val intersectionSize = intersect(intervals(timex1, window), intervals(timex2, window)).map(size).sum

    // return the precision and recall
    (intersectionSize / size(timex2), intersectionSize / size(timex1))
  }

  def intervals(timex: TimeExpression, window: Interval): Seq[Interval] = timex match {
    case interval: Interval => Seq(interval)
    case intervals: Intervals => intervals
    case rInterval: RepeatingInterval =>
      val start = rInterval.preceding(window.start).next.end
      rInterval.following(start).takeWhile(_.start isBefore window.end).toSeq
    case _ => Seq.empty
  }

  def size(timex: TimeExpression): Double = timex match {
    case interval: Interval => epoch(interval.end) - epoch(interval.start)
    case intervals: Intervals => intervals.map(size).sum
    case _: RepeatingInterval => Double.PositiveInfinity
    case _ => 0
  }

  implicit val ldtOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isBefore _)
  implicit val intervalOrdering: Ordering[Interval] = Ordering.by(i => (i.start, i.end))

  def intersect(intervals1: Seq[Interval], intervals2: Seq[Interval]): Seq[Interval] = {
    import ldtOrdering.mkOrderingOps
    // finds all overlaps between the two sets of intervals
    intervals1.flatMap(i1 => intervals2.flatMap(i2 => if (i1.start < i2.end && i2.start < i1.end) {
      Seq(SimpleInterval(i1.start max i2.start, i1.end min i2.end))
    } else {
      Seq.empty[Interval]
    })).sorted.foldLeft[List[Interval]](Nil) {
      // always add the first interval to the result
      case (Nil, next) => List(next)
      // if the next interval overlaps with the last, merge it, otherwise, add it
      case (last :: rest, next) => if (last.end < next.start) {
        next :: last :: rest
      } else {
        SimpleInterval(last.start, next.end max last.end) :: rest
      }
    }.reverse
  }

  val skip = Set("NotNormalizable", "Frequency", "PreAnnotation")

  def main(args: Array[String]): Unit = {
    val dctDir = args(0)
    val outDir = args(1)
    val Array(dir) = args.drop(2)
    var sum_precision: Double = 0
    var sum_recall: Double = 0
    var sum_cases = 0
    var sum_gs = 0
    var sum_sys = 0
    for (xmlFile <- allTimeNormFiles(new File(dir))) {
      val fileName = xmlFile.getName.replaceAll("[.][^.]*[.][^.]*[.][^.]*.xml", "")
      val dctPath = dctDir + "/" + fileName + "/" + fileName + ".dct"
      val outPath = outDir + "/" + fileName
      printf("Document: %s\n",fileName)

      val dctString = io.Source.fromFile(dctPath).getLines.toList(0)
      val dct: Interval = parseDCT(dctString)
      printf("DCT: %s\n\n",dctString)

      println("Intervals in Gold:")
      try {
        var gs: List[Tuple3[Entity, TimeExpression, Seq[Interval]]] = Nil
        val gsdata = Data.fromPaths(xmlFile.getPath, None)
        implicit var data = gsdata
        var aReader = new AnaforaReader(dct)
        for (entity <- data.topEntities.sortBy(_.fullSpan); if !skip.contains(entity.`type`)) {
          try {
            val temporal = aReader.temporal(entity)
            if (temporal.isInstanceOf[Interval] || temporal.isInstanceOf[Intervals]) {
              val intervals = get_intervals(temporal)
              intervals.map(i => printf("  %s [%s, %s) \n", entity.id, i.start, i.end))
              gs :+= (entity, temporal, intervals)
            }
          } catch {
            case ex: IllegalArgumentException => println(ex)
            case ex: NumberFormatException => println(ex)
            case ex: AssertionError => println(ex)
            case ex: NotImplementedError => println(ex)
            case ex: Exception => println(ex)
          }
        }
        println()
        sum_gs += gs.length

        println("Intervals in Answer:")
        var sys: List[Tuple3[Entity, TimeExpression, Seq[Interval]]] = Nil
        val outFile = allTimeNormFiles(new File(outPath))(0)
        val outFilePath = outPath + "/" + outFile.getName
        val sysdata = Data.fromPaths(outFilePath, None)
        data = sysdata
        aReader = new AnaforaReader(dct)
        for (entity <- data.topEntities.sortBy(_.expandedSpan); if !skip.contains(entity.`type`)) {
          try {
            val temporal = aReader.temporal(entity)
            if (temporal.isInstanceOf[Interval] || temporal.isInstanceOf[Intervals]) {
              val intervals = get_intervals(temporal)
              intervals.map(i => printf("  %s [%s, %s) \n", entity.id, i.start, i.end))
              sys :+= (entity, temporal, intervals)
            }
          } catch {
            case ex: IllegalArgumentException => println(ex)
            case ex: NumberFormatException => println(ex)
            case ex: AssertionError => println(ex)
            case ex: NotImplementedError => println(ex)
            case ex: Exception => println(ex)
          }
        }
        println()
        sum_sys += sys.length

        println("Intersections:")
        for (gstimex <- gs) {
          val gsentity = gstimex._1
          var max_recall = 0.0
          try {
          for (systimex <- sys) {
            val sysentity = systimex._1
            if (gsentity.expandedSpan._1 <= sysentity.expandedSpan._2 && gsentity.expandedSpan._2 >= sysentity.expandedSpan._1) {
              try {
                data = gsdata
                printf("  Gold: %s \"%s\"\n", gsentity.id, gsentity.expandedSpan)
                printf("\t%s\n", gstimex._3)
                data = sysdata
                printf("  Answ: %s \"%s\"\n", sysentity.id, sysentity.expandedSpan)
                printf("\t%s\n", systimex._3)
                val (precision, recall) = score(gstimex._2, systimex._2)
                printf ("  Precision: %.3f\tRecall: %.3f", precision, recall)
                sum_precision += precision
                if (recall > max_recall) max_recall = recall
                sum_cases += 1
              } catch {
                case ex: IllegalArgumentException => println(ex)
                case ex: NotImplementedError => println(ex)
              }
              println()
            }
          }
          } catch {
            case ex: Exception => println(ex)
          }
          sum_recall += max_recall
        }
      } catch {
        case ex: MatchError => ex.printStackTrace
        case ex: FileNotFoundException => println(ex)
      }
    }

    val precision = sum_precision / sum_sys
    val recall = sum_recall / sum_gs
    val fscore = 2 * precision * recall / (precision + recall)
    printf("Gold cases: %d\n",sum_gs)
    printf("Sys cases: %d\n",sum_sys)
    printf("Intersec cases: %d\n", sum_cases)
    printf("Precision: %.3f\n", precision)
    printf("Recall: %.3f\n", recall)
    printf("F1: %.3f\n", fscore)
  }


  def allTimeNormFiles(dir: File): Array[File] = {
    val files = dir.listFiles()
    val xmlFiles = files.filter(_.getName.matches(".*[.]xml"))
    val subFiles = files.filter(_.isDirectory).flatMap(allTimeNormFiles)
    xmlFiles ++ subFiles
  }

}