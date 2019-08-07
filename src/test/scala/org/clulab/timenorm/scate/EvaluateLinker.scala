package org.clulab.timenorm.scate

import java.nio.file.{Path, Paths}

import scala.xml.Elem
import org.clulab.anafora.{Anafora, Data, Entity}

object EvaluateLinker {

  private def linkedEntityInfo(inRoots: Array[Path], exclude: Set[String]): Array[(Entity, String, Entity, Int)] = {
    for {
      inRoot <- inRoots
      xmlPath <- Anafora.xmlPaths(inRoot, "glob:**TimeNorm.gold.completed.xml")
      data = Data.fromPaths(xmlPath)
      entity1 <- data.entities
      if !exclude.contains(entity1.`type`)
      propertyName <- entity1.properties.names
      entity2 <- entity1.properties.getEntities(propertyName)(data)
      if !exclude.contains(entity2.`type`)
      (start1, end1) = entity1.fullSpan
      (start2, end2) = entity2.fullSpan
    } yield {
      val dist = Seq(start1 - start2, start1 - end2, end1 - start2, end1 - end2).minBy(math.abs)
      (entity1, propertyName, entity2, dist)
    }
  }

  def entityDistanceHistogram(inRoots: Array[Path], exclude: Set[String] = Set.empty): Map[Int, Int] = {
      linkedEntityInfo(inRoots, exclude).map(_._4).groupBy(identity).mapValues(_.length).toMap
  }

  def distancesByTypes(inRoots: Array[Path], exclude: Set[String] = Set.empty): Map[String, Map[Int, Int]] = {
    val distances = for ((entity1, propertyName, _, dist) <- linkedEntityInfo(inRoots, exclude)) yield {
      (s"${entity1.`type`}:$propertyName", dist)
    }
    distances.toSeq.groupBy(_._1).mapValues(_.map(_._2).groupBy(identity).mapValues(_.length).toMap).toMap
  }

  def evaluateLinker(inRoots: Array[Path], verbose: Boolean = false): (Int, Int, Int) = {
    val parser = new TemporalNeuralParser()
    val results = for {
      inRoot <- inRoots
      xmlPath <- Anafora.xmlPaths(inRoot, "glob:**TimeNorm.gold.completed.xml")
      data = Data.fromPaths(xmlPath)
      entities = data.entities.sortBy(_.fullSpan._1)
      timeSpans = entities.map(e => (e.fullSpan._1, e.fullSpan._2, e.`type`))
      (i, links) <- entities.indices zip parser.inferLinks(timeSpans.toArray)
    } yield {
      val goldProperties = entities(i).properties.xml.child.collect{
        case elem: Elem if elem.text.contains("@") => (elem.label, elem.text)
      }.toSet
      val systemProperties = links.map{ case (name, j) => (name, entities(j).id)}.toSet
      val correct = goldProperties.intersect(systemProperties).size
      if (verbose && (correct != goldProperties.size || correct != systemProperties.size)) {
        val additions = systemProperties.diff(goldProperties)
        val deletions = goldProperties.diff(systemProperties)
        for ((items, flag) <- Seq((additions, "+"), (deletions, "-")); (name, id) <- items) {
          println(f"$flag ${entities(i).`type` + ":" + name}%-35s ${entities(i).id}%35s -> $id%35s at ${entities(i).fullSpan}%15s in $xmlPath")
        }
      }
      (correct, goldProperties.size, systemProperties.size)
    }
    val (correctCounts, goldCounts, systemCounts) = results.toSeq.unzip3
    (correctCounts.sum, goldCounts.sum, systemCounts.sum)
  }

  private def printHistogram(histogram: Map[Int, Int]): Unit = {
    for ((distance, count) <- histogram.toSeq.sortBy(_._1)) {
      println(f"$distance%4d $count%4d")
    }
  }

  def main(args: Array[String]): Unit = {
    val paths = args.map(Paths.get(_))

    println("## Histogram of distances between linked entities ##")
    printHistogram(entityDistanceHistogram(paths, exclude = Set("Event")))
    println()

    println("## Histogram of distances by link type ##")
    for ((name, distances) <- distancesByTypes(paths).toSeq.sortBy(_._1)) {
      println(name)
      printHistogram(distances)
    }
    println()

    val (nCorrect, nGold, nSystem) = evaluateLinker(paths, verbose = true)
    val nCorrectF = nCorrect.toFloat
    println(
      f"""
        |## Linking performance ##
        |Correct:   $nCorrect
        |Gold:      $nGold
        |System:    $nSystem
        |Precision: ${nCorrectF / nSystem}%.3f
        |Recall:    ${nCorrectF / nGold}%.3f
        |""".stripMargin)
  }
}
