package org.clulab.timenorm.scate

import java.nio.file.Paths

import org.clulab.anafora.{Anafora, Data}

object ParseAnaforaXMLs {

  // annotations that don't represent normalizable time expressions
  private val skip = Set("NotNormalizable", "Frequency", "PreAnnotation")

  def main(args: Array[String]): Unit = args match {
    case Array(dir) =>
      for (xmlPath <- Anafora.xmlPaths(Paths.get(dir), "glob:**.TimeNorm.gold.completed.xml")) {
        val textPath = xmlPath.getParent.resolve(xmlPath.getParent.getFileName)
        println(xmlPath)
        implicit val data: Data = Data.fromPaths(xmlPath, Some(textPath))
        val reader = new AnaforaReader(SimpleInterval.of(2010, 10, 10))
        for (entity <- data.entities.sortBy(_.fullSpan); if !skip.contains(entity.`type`)) {
          printf("\"%s\"[%s] ", entity.text, entity.spans.map(t => "%s,%s".format(t._1, t._2)).sorted.mkString(";"))
          try {
            val temporal = reader.temporal(entity)
            printf("%s ", temporal)
            if (temporal.isDefined) temporal match {
              case Interval(start, end) => printf("[%s, %s) ", start, end)
              case _ =>
            }
            println()
          } catch {
            case e @ (_: Anafora.Exception | _: AnaforaReader.Exception | _: UnsupportedOperationException) =>
              e.printStackTrace() // print error and continue
          }
        }
      }
  }
}
