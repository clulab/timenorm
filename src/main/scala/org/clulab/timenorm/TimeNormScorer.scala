package org.clulab.timenorm

import org.clulab.timenorm.formal.{Interval, TimeExpression}
import org.clulab.timenorm.formal._

import org.clulab.anafora.Data
import java.io.File
import java.time.{Instant, LocalDateTime, ZoneId}

import scala.util.Try


object TimeNormScorer {

  case class Timex(id: String, textSpan: (Int, Int), time: TimeExpression) {
    val (textStart, textEnd) = textSpan

    def textOverlaps(that: Timex): Boolean = {
      this.textStart < that.textEnd && that.textStart < this.textEnd
    }
  }
  object Timex {
    def allFrom(reader: AnaforaReader)(implicit data: Data): Seq[Timex] = {
      data.topEntities.filter(e => !skip.contains(e.`type`)).flatMap(e =>
        Try(Timex(e.id, e.fullSpan, reader.temporal(e))).fold(ex => {ex.printStackTrace(); Seq.empty}, ts => Seq(ts)))
    }
    def allIntervalsFrom(reader: AnaforaReader)(implicit data: Data): Seq[Timex] = {
      allFrom(reader)(data).filter(t => Try(intervals(t.time).size > 0 && intervals(t.time).forall(i => i.isDefined)).getOrElse(false))
    }
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

  def score(gold: TimeExpression, system: TimeExpression): (Double, Double) = {
    val intersectionSize = intervalIntersection(Seq(gold), Seq(system)).map(size).sum
    (intersectionSize / size(system), intersectionSize / size(gold))
  }

  def intervalPrecision(gold: Seq[TimeExpression], system: Seq[TimeExpression]): Double = {
    val intersectionSize = intervalIntersection(gold, system).map(size).sum
    if (intersectionSize == 0) 0.0 else intersectionSize / system.map(size).sum
  }

  def intervalScores(goldTimexes: Seq[Timex],
                     systemTimexes: Seq[Timex],
                     verbose: Boolean = false): (Seq[Double], Seq[Double]) = {
    val precisions = for (systemTimex <- systemTimexes) yield {
      val overlappingTimexes = goldTimexes.filter(systemTimex.textOverlaps)
      val precision = intervalPrecision(overlappingTimexes.map(_.time), Seq(systemTimex.time))
      if (verbose) {
        println(s"  Gold: $overlappingTimexes ${overlappingTimexes.map(_.time).map(intervals)}")
        println(s"  Answ: $systemTimex ${intervals(systemTimex.time)}")
        printf("  Precision: %.3f\n", precision)
      }
      precision
    }
    val recalls = for (goldTimex <- goldTimexes) yield {
      val overlappingTimexes = systemTimexes.filter(goldTimex.textOverlaps)
      val recall = intervalPrecision(overlappingTimexes.map(_.time), Seq(goldTimex.time))
      if (verbose) {
        println(s"  Gold: $goldTimex ${intervals(goldTimex.time)}")
        println(s"  Answ: $overlappingTimexes ${overlappingTimexes.map(_.time).map(intervals)}")
        printf("  Recall: %.3f\n", recall)
      }
      recall
    }
    (precisions, recalls)
  }

  implicit val ldtOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isBefore _)
  implicit val intervalOrdering: Ordering[Interval] = Ordering.by(i => (i.start, i.end))

  def intervalIntersection(timexes1: Seq[TimeExpression], timexes2: Seq[TimeExpression]): Seq[Interval] = {
    // find the earliest interval start and the latest interval end (so RepeatingIntervals can be bounded)
    val intervalUnion = timexes1 ++ timexes2 flatMap intervals
    val window = SimpleInterval(intervalUnion.map(_.start).min, intervalUnion.map(_.end).min)

    // intersect the intervals (now including those from RepeatingIntervals) from each set of time expressions
    val intervals1 = timexes1.flatMap(t => intervals(t, window))
    val intervals2 = timexes2.flatMap(t => intervals(t, window))

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

  def intervals(timex: TimeExpression): Seq[Interval] = timex match {
    case interval: Interval => List(interval)
    case intervals: Intervals => intervals
    case _ => Seq.empty
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

  val skip = Set("NotNormalizable", "Frequency", "PreAnnotation")

  def main(args: Array[String]): Unit = {
    val Array(dctDir, outDir, dir) = args
    var sum_gs = 0
    var sum_sys = 0
    val precisionRecalls = for (xmlFile <- allTimeNormFiles(new File(dir))) yield {
      val fileName = xmlFile.getName.replaceAll("[.][^.]*[.][^.]*[.][^.]*.xml", "")
      val dctPath = dctDir + "/" + fileName + "/" + fileName + ".dct"
      val outPath = outDir + "/" + fileName
      printf("Document: %s\n",fileName)

      val dctString = io.Source.fromFile(dctPath).getLines.toList(0)
      val dct: Interval = parseDCT(dctString)
      printf("DCT: %s\n\n",dctString)

      val goldData = Data.fromPaths(xmlFile.getPath, None)
      val goldTimexes = Timex.allIntervalsFrom(new AnaforaReader(dct)(goldData))(goldData)
      sum_gs += goldTimexes.size

      println("Intervals in Gold:")
      for (timex <- goldTimexes; interval <- intervals(timex.time)) {
        println(s"  ${timex.id} [${interval.start}, ${interval.end})")
      }

      val outFile = allTimeNormFiles(new File(outPath))(0)
      val outFilePath = outPath + "/" + outFile.getName
      val systemData = Data.fromPaths(outFilePath, None)
      val systemTimexes = Timex.allIntervalsFrom(new AnaforaReader(dct)(systemData))(systemData)
      sum_sys += systemTimexes.size

      println("Intervals in Answer:")
      for (timex <- systemTimexes; interval <- intervals(timex.time)) {
        println(s"  ${timex.id} [${interval.start}, ${interval.end})")
      }

      println("Intersections:")
      intervalScores(goldTimexes, systemTimexes)
    }
    val (docPrecisions, docRecalls) = precisionRecalls.unzip
    val precisions = docPrecisions.flatten
    val recalls = docRecalls.flatten
    val precision = precisions.sum / precisions.length
    val recall = recalls.sum / recalls.length
    val fscore = 2 * precision * recall / (precision + recall)
    printf("Gold cases: %d\n",sum_gs)
    printf("Sys cases: %d\n",sum_sys)
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
