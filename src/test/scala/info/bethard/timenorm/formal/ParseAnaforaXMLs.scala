package info.bethard.timenorm.formal

import java.io.File

import info.bethard.anafora.Data

object ParseAnaforaXMLs {

  // annotations that don't represent normalizable time expressions
  val skip = Set("NotNormalizable", "Frequency", "PreAnnotation")

  def main(args: Array[String]): Unit = {
    val Array(dir) = args
    for (xmlFile <- allTimeNormFiles(new File(dir))) {
      val textPath = xmlFile.getPath.replaceAll("[.][^.]*[.][^.]*[.][^.]*.xml", "")
      println(xmlFile)
      implicit val data = Data.fromPaths(xmlFile.getPath, textPath)
      for (entity <- data.entities.sortBy(_.fullSpan); if !skip.contains(entity.`type`)) {
        printf("\"%s\"[%s] ", entity.text, entity.spans.map(t => "%s,%s".format(t._1, t._2)).toSeq.sorted.mkString(";"))
        val temporal = AnaforaReader.temporal(entity)
        printf("%s ", temporal)
        if (temporal.isDefined) temporal match {
          case Interval(start, end) => printf("[%s, %s) ", start, end)
          case _ =>
        }
        println()
      }
    }
  }

  def allTimeNormFiles(dir: File): Array[File] = {
    val files = dir.listFiles()
    val xmlFiles = files.filter(_.getName.matches(".*[.]TimeNorm[^.]*[.]gold[.]completed[.]xml"))
    val subFiles = files.filter(_.isDirectory).flatMap(allTimeNormFiles)
    xmlFiles ++ subFiles
  }

}
