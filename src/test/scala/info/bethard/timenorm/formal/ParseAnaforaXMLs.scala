package info.bethard.timenorm.formal

import java.io.File

import info.bethard.anafora.Data

object ParseAnaforaXMLs {

  // annotations that don't represent normalizable time expressions
  val skip = Set("NotNormalizable", "Frequency")

  def main(args: Array[String]): Unit = {
    val Array(dir) = args
    for (xmlFile <- allTimeNormFiles(new File(dir))) {
      val textPath = xmlFile.getPath.replaceAll("[.][^.]*[.][^.]*[.][^.]*.xml", "")
      println(xmlFile)
      implicit val data = Data.fromPaths(xmlFile.getPath, textPath)
      for (entity <- data.entities.sortBy(_.fullSpan); if !skip.contains(entity.`type`)) {
        printf("\"%s\"[%s] ", entity.text, entity.spans.map(t => "%s,%s".format(t._1, t._2)).toSeq.sorted.mkString(";"))
        println(AnaforaReader.temporal(entity))
      }
    }
  }

  def allTimeNormFiles(dir: File): Array[File] = {
    val files = dir.listFiles()
    val xmlFiles = files.filter(_.getName.matches(".*[.]TimeNorm[^.]*[.](?!preannotation)[^.]*[.][^.]*.xml"))
    val subFiles = files.filter(_.isDirectory).flatMap(allTimeNormFiles)
    xmlFiles ++ subFiles
  }

}
