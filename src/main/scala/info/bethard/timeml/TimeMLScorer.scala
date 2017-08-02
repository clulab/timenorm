package info.bethard.timeml

import java.io.{File, FileNotFoundException}
import java.time.format.DateTimeParseException

import com.codecommit.antixml._
import com.codecommit.antixml.{text => ElemText}
import info.bethard.timeml.TimeMLanaforaDocument._
import info.bethard.timenorm.TemporalExpressionParser
import info.bethard.timenorm.TimeSpan
import info.bethard.timenorm.formal.{Interval, Intervals, ThisRIs, TimeExpression, _}
import java.time.temporal.ChronoField._
import java.time.temporal.ChronoUnit._

import info.bethard.anafora.{Annotation, Data, Entity, Properties}
import java.io.File
import java.time.ZoneId
import java.time.temporal.TemporalUnit
import java.time.LocalDateTime
import java.time.Instant

object TimeMLScorer {

  def get_intervals(timex: TimeExpression): Seq[Interval] = {

    var intervals: Seq[Interval] =
      if (timex.isInstanceOf[Interval]) {
        println(timex, timex.asInstanceOf[Interval].start)
        List(SimpleInterval(timex.asInstanceOf[Interval].start, timex.asInstanceOf[Interval].end)) }
      else if (timex.isInstanceOf[Intervals])
        timex.asInstanceOf[Intervals].iterator.toList
      else if (timex.isInstanceOf[RepeatingInterval])
        Nil
      else {
        printf("%s is not a valid TimeExpression class\n", timex.getClass.getSimpleName)
        //System.err.printf("%s is not a valid TimeExpression class\n", timex.getClass.getSimpleName)
        throw new IllegalArgumentException
      }
    return intervals
  }

  def get_intervals(timespan: TimeSpan): Seq[Interval] = {
    var intervals: Seq[Interval] = List(SimpleInterval(timespan.start.toLocalDateTime(), timespan.end.toLocalDateTime()))
    return intervals
  }

  def epoch(datetime: java.time.LocalDateTime): Long = datetime.atZone(ZoneId.systemDefault).toEpochSecond
  def datetime(epoch: Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochSecond(epoch), ZoneId.systemDefault())

  def get_range_limmits(range: TemporalUnit) : Tuple2[LocalDateTime, LocalDateTime] = range.toString match {
    case "Years" => (LocalDateTime.now.withDayOfYear(1), LocalDateTime.now.plusYears(1).withDayOfYear(1))
    case "Hours" => (LocalDateTime.now.withHour(1), LocalDateTime.now.plusDays(1).withHour(1))
    case "Minutes" => (LocalDateTime.now.withMinute(1), LocalDateTime.now.plusHours(1).withMinute(1))
    case _ => (LocalDateTime.now.withDayOfMonth(1), LocalDateTime.now.plusMonths(1).withDayOfMonth(1))
  }

  def parseDCT(dctString: String): Seq[Int] = {
    val datetime = dctString.split("T")
    if (datetime.size == 2) {
      val YMD = datetime(0).split("-").map(_.toString.toInt)
      val HMS = datetime(1).split(":").map(_.toString.toInt)
      if (YMD.size < 3 || HMS.size == 0)
        throw new  Exception("DCT malformed")
      if (HMS.size == 3)
        //return Seq(YMD(0),YMD(1),YMD(2),HMS(0),HMS(1),HMS(2))
        return Seq(YMD(0),YMD(1),YMD(2))
      else if (HMS.size == 2)
        //return Seq(YMD(0),YMD(1),YMD(2),HMS(0),HMS(1))
        return Seq(YMD(0),YMD(1),YMD(2))
      else
        //return Seq(YMD(0),YMD(1),YMD(2),HMS(0))
        return Seq(YMD(0),YMD(1),YMD(2))
    } else {
      val YMD = datetime(0).split("-").map(_.toString.toInt)
      if (YMD.size == 0)
        throw new  Exception("DCT malformed")
      if (YMD.size == 3)
        return Seq(YMD(0),YMD(1),YMD(2))
      else if (YMD.size == 2)
        return Seq(YMD(0),YMD(1))
      else
        return Seq(YMD(0))
    }
  }


  def compact_intervals(intervals: Seq[Interval]): Seq[Interval] = {
    var compactIntervals: Seq[Interval] = Seq ()
    if (intervals.size != 0) {
      for (Interval (start, end) <- intervals) {
        var newCompact: Seq[Interval] = Seq ()
        var startZone = epoch (start)
        var endZone = epoch (end)

        for (Interval (cStart, cEnd) <- compactIntervals) {
          val cStartZone = epoch (cStart)
          val cEndZone = epoch (cEnd)
          val maxStart = Math.max (startZone, cStartZone)
          val minEnd = Math.min (endZone, cEndZone)

          if (minEnd > maxStart) {
            val minStart = Math.min (startZone, cStartZone)
            val maxEnd = Math.max (endZone, cEndZone)
            startZone = minStart
            endZone = maxEnd
          } else {
            newCompact :+= SimpleInterval (cStart, cEnd)
          }
        }
        newCompact :+= SimpleInterval (datetime (startZone), datetime (endZone) )
        compactIntervals = newCompact
      }
    }
    return compactIntervals
  }

  def score(gsTimex: TimeExpression, gsIntervs: Seq[Interval], sysTimex: TimeSpan, sysIntervs: Seq[Interval]): (Double, Double) = {

    var gsIntervals = gsIntervs
    var sysIntervals = sysIntervs

    var gstotal : Double = 0
    var systotal : Double = 0
    if (gsIntervals.size == 0 && sysIntervals.size == 0) {
      val gsRange = gsTimex.asInstanceOf[RepeatingInterval].range
      val sysRange = sysTimex.asInstanceOf[RepeatingInterval].range
      val range = gsRange.getDuration.getSeconds > sysRange.getDuration.getSeconds match {
        case true => gsRange
        case false => sysRange
      }
      val (rangeStart, rangeEnd) : (LocalDateTime, LocalDateTime) = get_range_limmits(range)
      val gsFollowing = gsTimex.asInstanceOf[RepeatingInterval].following(rangeStart)
      var gsNext = gsFollowing.next
      while (epoch(gsNext.start) < epoch(rangeEnd)) {
        gsIntervals :+= gsNext
        gsNext = gsFollowing.next
      }
      val sysFollowing = sysTimex.asInstanceOf[RepeatingInterval].following(rangeStart)
      var sysNext = sysFollowing.next
      while (epoch(sysNext.start) < epoch(rangeEnd)) {
        sysIntervals :+= sysNext
        sysNext = sysFollowing.next
      }
      gstotal = Double.PositiveInfinity
      systotal = Double.PositiveInfinity
    }

    if (sysIntervals.size != 0) {
      sysIntervals = compact_intervals(sysIntervals)
    }
    if (gsIntervals.size != 0) {
      gsIntervals = compact_intervals(gsIntervals)
    }

    if (sysIntervals.size != 0) {
      for (Interval(sysStart, sysEnd) <- sysIntervals) {
        val sysStartZone = epoch(sysStart)
        val sysEndZone = epoch(sysEnd)
        systotal += (sysEndZone - sysStartZone)
      }
    } else if (gsIntervals.size != 0) {
      val sysFollowing = sysTimex.asInstanceOf[RepeatingInterval].following(gsIntervals(0).start)
      var sysNext = sysFollowing.next
      while (epoch(sysNext.start) < epoch(gsIntervals.last.end)) {
        sysIntervals :+= sysNext
        sysNext = sysFollowing.next
      }
      systotal = Double.PositiveInfinity
    }

    if (gsIntervals.size != 0) {
      for (Interval(gsStart, gsEnd) <- gsIntervals) {
        val gsStartZone = epoch(gsStart)
        val gsEndZone = epoch(gsEnd)
        gstotal += (gsEndZone - gsStartZone)
      }
    } else if (sysIntervals.size != 0) {
      val gsFollowing = gsTimex.asInstanceOf[RepeatingInterval].following(sysIntervals(0).start)
      var gsNext = gsFollowing.next
      while (epoch(gsNext.start) < epoch(sysIntervals.last.end)) {
        gsIntervals :+= gsNext
        gsNext = gsFollowing.next
      }
      gstotal = Double.PositiveInfinity
    }

    var intersec : Double = 0
    for (Interval(gsStart, gsEnd) <- gsIntervals) {
      val gsStartZone = epoch(gsStart)
      val gsEndZone = epoch(gsEnd)
      for (Interval(sysStart, sysEnd) <- sysIntervals) {
        val sysStartZone = epoch(sysStart)
        val sysEndZone = epoch(sysEnd)
        val maxStart = Math.max(gsStartZone, sysStartZone)
        val minEnd = Math.min(gsEndZone, sysEndZone)
        if (minEnd > maxStart) intersec += (minEnd - maxStart)
      }
    }

    val P : Double = intersec/systotal
    val R : Double = intersec/gstotal

    printf ("  Precision: %.3f\tRecall: %.3f",P,R)
    return (P, R)
  }


  val skip = Set("NotNormalizable", "Frequency", "PreAnnotation")

  def main(args: Array[String]): Unit = {
    val textDir = args(0)
    val dctDir = args(1)
    val outDir = args(2)
    val Array(dir) = args.drop(3)
    var sum_precision: Double = 0
    var sum_recall: Double = 0
    var sum_cases = 0
    var sum_gs = 0
    var sum_sys = 0
    for (xmlFile <- allTimeNormFiles(new File(dir))) {
      val fileName = xmlFile.getName.replaceAll("[.][^.]*[.][^.]*[.][^.]*.xml", "")
      val textPath = textDir + "/" + fileName
      val dctPath = dctDir + "/" + fileName
      val outPath = outDir + "/" + fileName
      printf("Document: %s\n",fileName)

      val dctString = io.Source.fromFile(dctPath).getLines.toList(0)
      val dct: Seq[Int] = parseDCT(dctString)
      printf("DCT: %s\n\n",dctString)

      println("Intervals in Gold:")
      try {
        var gs: List[Tuple3[Entity, TimeExpression, Seq[Interval]]] = Nil
        val gsdata = Data.fromPaths(xmlFile.getPath, textPath)
        implicit var data = gsdata
        var aReader = new AnaforaReader(dct)
        for (entity <- data.topEntities.sortBy(_.expandedSpan); if !skip.contains(entity.`type`)) {
          try {
            val temporal = aReader.temporal(entity)
            if (temporal.isInstanceOf[Interval] || temporal.isInstanceOf[Intervals]) {
              val intervals = get_intervals(temporal)
              intervals.map(i => printf("  %s [%s, %s) \n", entity.id, i.start, i.end))
              gs :+= (entity, temporal, intervals)
            }
          } catch {
            case ex: IllegalArgumentException => println(entity.id, entity.expandedText, ex)
            case ex: NumberFormatException => println(entity.id, entity.expandedText, ex)
            case ex: AssertionError => println(entity.id, entity.expandedText, ex)
            case ex: NotImplementedError => println(entity.id, entity.expandedText, ex)
            case ex: Exception => println(entity.id, entity.expandedText, ex)

          }
        }
        println()
        sum_gs += gs.length

        println("Intervals in Answer:")
        var sys: List[Tuple3[TIMEX, TimeSpan, Seq[Interval]]] = Nil
        for (xmlFile <- allTimeNormFiles(new File(outPath))) {
          val timeMLanafora = new TimeMLanaforaDocument(xmlFile)
          for (timex <- timeMLanafora.timeExpressions.sortBy(_.fullSpan)) {
            try {

              if (!(timex.value(0).isEmpty ||
                    timex.value(0).startsWith("P") ||
                    timex.value(0).startsWith("T") ||
                    timex.value(0).contains('X'))) {
                val rmTimeZone = raw"(:\d{2})-\d+".r
                val value = rmTimeZone.replaceAllIn(timex.value(0),"$1")
                val timeSpan = TimeSpan.fromTimeMLValue(value)
                val intervals = get_intervals(timeSpan.asInstanceOf[TimeSpan])
                sys :+= ((timex.asInstanceOf[TIMEX], timeSpan.asInstanceOf[TimeSpan], intervals))
                printf("  %s [%s, %s) \n", timex.id, timeSpan.start, timeSpan.end)
              }
            } catch {
              case ex: Exception => //println(timex.id, timex.text, ex)
              case ex: NotImplementedError => println(timex.id, timex.text, ex)

            }
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
              if (gsentity.expandedSpan._1 <= sysentity.fullSpan._2 && gsentity.expandedSpan._2 >= sysentity.fullSpan._1) {
                try {
                  printf("  Gold: %s \"%s\" %s \n", gsentity.id, gsentity.expandedSpan, gsentity.expandedText)
                  printf("\t%s\n", gstimex._2)
                  printf("\t%s\n", gstimex._3)
                  printf("  Answ: %s \"%s\" %s \n", sysentity.id, sysentity.fullSpan, sysentity.text)
                  printf("\t%s\n", systimex._3)
                  val (precision, recall) = score(gstimex._2, gstimex._3, systimex._2, systimex._3)
                  println()
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
        case ex: MatchError => println(ex)
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
