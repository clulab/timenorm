package org.clulab.timeml

import scala.collection.JavaConverters._
import org.clulab.timenorm.scate.{AnaforaReader, Interval, Intervals, SimpleInterval, TimeExpression, TimeNormScorer}
import org.clulab.anafora.{Anafora, Data, Entity}
import java.nio.file.{FileSystems, Files, Path, Paths}

import org.clulab.timenorm.scfg.TimeSpan

object TimeMLScorer {


  private val skip = Set("NotNormalizable", "Frequency", "PreAnnotation")

  def main(args: Array[String]): Unit = args.map(Paths.get(_)) match {
    case Array(dctDir, systemDir, goldDir) =>
      var sumPrecision = 0.0
      var sumRecall = 0.0
      var sumIntersection = 0
      var sumGold = 0
      var sumSystem = 0
      for (xmlPath <- Anafora.xmlPaths(goldDir)) {
        val subDir = goldDir.relativize(xmlPath.getParent)
        val dctPath = dctDir.resolve(subDir).resolve(subDir.getFileName.toString +".dct")
        val List(systemPath) = Anafora.xmlPaths(systemDir.resolve(subDir)).toList
        println(s"Document: $subDir")

        val dctString = new String(Files.readAllBytes(dctPath)).trim
        println(s"DCT: $dctString\n")
        val dct: Interval = TimeNormScorer.parseDCT(dctString)

        println("Intervals in Gold:")
        var goldItems: List[(Entity, TimeExpression, Seq[Interval])] = Nil
        implicit val goldData: Data = Data.fromPaths(xmlPath)
        val reader = new AnaforaReader(dct)
        for (entity <- goldData.topEntities.sortBy(_.expandedSpan); if !skip.contains(entity.`type`)) {
          try {
            reader.temporal(entity) match {
              case i@(_: Interval | _: Intervals) if i.isDefined =>
                val intervals = TimeNormScorer.intervals(i)
                if (intervals.nonEmpty) {
                  for (i <- intervals) {
                    println(s"  ${entity.id} [${i.start}, ${i.end})")
                  }
                  goldItems :+= (entity, i, intervals)
                }
              case _ => // ignore all other types of time expressions
            }
          } catch {
            case e: AnaforaReader.Exception => e.printStackTrace() // print error, but ignore
          }
        }
        println()
        sumGold += goldItems.length


        println("Intervals in Answer:")
        var systemItems: List[(Entity, TimeExpression, Seq[Interval])] = Nil
        val systemData = Data.fromPaths(systemPath)
        for (timex <- systemData.entities.sortBy(_.fullSpan); if timex.`type` == "TIMEX3") {
          val valueOption = timex.properties.get("value")
          val modOption = timex.properties.get("mod")
          if (modOption.isEmpty) {
            for (value <- valueOption
                 if value.nonEmpty && !value.startsWith("P") && !value.startsWith("T") && !value.contains("X")) {
              val valueNoTimeZone = """(:\d{2})-\d+""".r.replaceAllIn(value, "$1")
              val timeSpan = TimeSpan.fromTimeMLValue(valueNoTimeZone)
              val interval = SimpleInterval(timeSpan.start.toLocalDateTime, timeSpan.end.toLocalDateTime)
              systemItems :+= ((timex, interval, Seq(interval)))
              println(s"  ${timex.id} [${timeSpan.start}, ${timeSpan.end})")
            }
          }
        }
        println()
        sumSystem += systemItems.length

        println("Intersections:")
        for ((goldEntity, goldTimex, goldIntervals) <- goldItems) {
          val recalls = for {
            (systemEntity, systemTimex, systemIntervals) <- systemItems
            if goldEntity.expandedSpan._1 <= systemEntity.fullSpan._2
            if goldEntity.expandedSpan._2 >= systemEntity.fullSpan._1
          } yield {
            println(s"""|  Gold:   ${goldEntity.id} "${goldEntity.expandedSpan}"
                        |    $goldTimex
                        |    ${goldIntervals.map(i => s"[${i.start}, ${i.end})")}
                        |  System: ${systemEntity.id} "${systemEntity.expandedSpan}"
                        |    $systemTimex
                        |    ${systemIntervals.map(i => s"[${i.start}, ${i.end})")}""".stripMargin)
            val (precision, recall) = TimeNormScorer.score(goldTimex, systemTimex)
            println(
              f"""|  Recall:    $recall%.3f
                  |  Precision: $precision%.3f
                  |""".stripMargin)
            sumPrecision += precision
            sumIntersection += 1
            recall
          }
          if (recalls.nonEmpty) {
            sumRecall += recalls.max
          }
        }
      }

      val precision = sumPrecision / sumSystem
      val recall = sumRecall / sumGold
      val f1 = 2 * precision * recall / (precision + recall)
      println(f"""|Gold cases:         $sumGold
                  |System cases:       $sumSystem
                  |Intersection cases: $sumIntersection
                  |Precision:          $precision%.3f
                  |Recall:             $recall%.3f
                  |F1:                 $f1%.3f""".stripMargin)
  }
}

