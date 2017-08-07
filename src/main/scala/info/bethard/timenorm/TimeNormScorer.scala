package info.bethard.timenorm

import info.bethard.timenorm.formal.{Interval, ThisRIs, TimeExpression}
import info.bethard.timenorm.formal._
import java.time.temporal.ChronoField._
import java.time.temporal.ChronoUnit._

import info.bethard.anafora.{Annotation, Data, Entity, Properties}
import java.io.File
import java.time.ZoneId
import java.time.temporal.TemporalUnit
import java.time.LocalDateTime

object TimeNormScorer {
  
  def get_intervals(timex: TimeExpression): Seq[Interval] = {

    var intervals: Seq[Interval] =
      if (timex.isInstanceOf[Interval])
        List(SimpleInterval(timex.asInstanceOf[Interval].start, timex.asInstanceOf[Interval].end))
      else if (timex.isInstanceOf[Intervals])
        timex.asInstanceOf[Intervals].iterator.toList
      else if (timex.isInstanceOf[RepeatingInterval])
        Nil
      else {
        printf("%s is not a valid TimeExpression class\n", timex.getClass.getSimpleName)
        throw new IllegalArgumentException
      }
    return intervals
  }

  def epoch(datetime: java.time.LocalDateTime): Long = datetime.atZone(ZoneId.systemDefault).toEpochSecond

  def get_range_limmits(range: TemporalUnit) : Tuple2[LocalDateTime, LocalDateTime] = range.toString match {
    case "Years" => (LocalDateTime.now.withDayOfYear(1), LocalDateTime.now.plusYears(1).withDayOfYear(1))
    case "Hours" => (LocalDateTime.now.withHour(1), LocalDateTime.now.plusDays(1).withHour(1))
    case "Minutes" => (LocalDateTime.now.withMinute(1), LocalDateTime.now.plusHours(1).withMinute(1))
    case _ => (LocalDateTime.now.withDayOfMonth(1), LocalDateTime.now.plusMonths(1).withDayOfMonth(1))
  }

  def score(gsTimex: TimeExpression, gsIntervs: Seq[Interval], sysTimex: TimeExpression, sysIntervs: Seq[Interval]): (Double, Double) = {
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

    print (P,R)
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
      val outPath = outDir + "/" + fileName + "/" + xmlFile.getName
      println(xmlFile)


      val dctString = io.Source.fromFile(dctPath).getLines.toList(0)
      val dctSeq: Seq[Int] = parseDCT(dctString)
      val dct: SimpleInterval = dctSeq.size match {
        case 1 => SimpleInterval.of(dctSeq(0))
        case 2 => SimpleInterval.of(dctSeq(0), dctSeq(1))
        case 3 => SimpleInterval.of(dctSeq(0), dctSeq(1), dctSeq(2))
        case 4 => SimpleInterval.of(dctSeq(0), dctSeq(1), dctSeq(2), dctSeq(3))
        case 5 => SimpleInterval.of(dctSeq(0), dctSeq(1), dctSeq(2), dctSeq(3), dctSeq(4))
        case 6 => SimpleInterval.of(dctSeq(0), dctSeq(1), dctSeq(2), dctSeq(3), dctSeq(4), dctSeq(5))
        case _ => throw new  Exception("DCT malformed")
      }
      printf("DCT: %s\n\n",dctString)

      println("Intervals in Gold:")
      try {
        var gs: List[Tuple3[Entity, TimeExpression, Seq[Interval]]] = Nil
        val gsdata = Data.fromPaths(xmlFile.getPath, textPath)
        implicit var data = gsdata
        val dctString = io.Source.fromFile(dctPath).getLines.toList(0)
        val dct = LocalDateTime.parse(dctString)
        var aReader = new AnaforaReader(dct)
        for (entity <- data.topEntities.sortBy(_.fullSpan); if !skip.contains(entity.`type`)) {
          try {
            val temporal = aReader.temporal(entity)
            if (temporal.isInstanceOf[Interval] || temporal.isInstanceOf[Intervals]) {
              val intervals = get_intervals(temporal)
              intervals.map(i => printf("%s [%s, %s) ", entity.id, i.start, i.end))
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

        var sys: List[Tuple3[Entity, TimeExpression, Seq[Interval]]] = Nil
        val sysdata = Data.fromPaths(outPath, textPath)
        data = sysdata
        aReader = new AnaforaReader(dct)
        for (entity <- data.topEntities.sortBy(_.expandedSpan); if !skip.contains(entity.`type`)) {
          try {
            val temporal = aReader.temporal(entity)
            if (temporal.isInstanceOf[Interval] || temporal.isInstanceOf[Intervals]) {
              val intervals = get_intervals(temporal)
              intervals.map(i => printf("%s [%s, %s) ", entity.id, i.start, i.end))
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

        for (gstimex <- gs) {
          val gsentity = gstimex._1
          var max_recall = 0.0
          try {
          for (systimex <- sys) {
            val sysentity = systimex._1
            if (gsentity.expandedSpan._1 <= sysentity.expandedSpan._2 && gsentity.expandedSpan._2 >= sysentity.expandedSpan._1) {
              try {
                data = gsdata
                printf("%s \"%s\" %s ", gsentity.id, gsentity.expandedText, gsentity.expandedSpan)
                printf("%s\n", gstimex._2)
                data = sysdata
                printf("%s \"%s\" %s ", sysentity.id, sysentity.expandedText, sysentity.expandedSpan)
                printf("%s\n", systimex._2)
                val (precision, recall) = score(gstimex._2, gstimex._3, systimex._2, systimex._3)
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
      }
    }

    val precision = sum_precision / sum_sys
    val recall = sum_recall / sum_gs
    val fscore = 2 * precision * recall / (precision + recall)
    printf("Gold cases: %d\n",sum_gs)
    printf("Sys cases: %d\n",sum_sys)
    printf("Intersec cases: %d\n", sum_cases)
    printf("Precision: %f\n", precision)
    printf("Recall: %f\n", recall)
    printf("F1: %f\n", fscore)
  }


  def allTimeNormFiles(dir: File): Array[File] = {
    val files = dir.listFiles()
    val xmlFiles = files.filter(_.getName.matches(".*[.]TimeNorm[^.]*[.]gold[.]completed[.]xml"))
    val subFiles = files.filter(_.isDirectory).flatMap(allTimeNormFiles)
    xmlFiles ++ subFiles
  }

}