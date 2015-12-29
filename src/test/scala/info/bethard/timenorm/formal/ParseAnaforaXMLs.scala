package info.bethard.timenorm.formal

import java.io.File

import info.bethard.anafora.Data

object ParseAnaforaXMLs {
  def main(args: Array[String]): Unit = {
    val Array(dir) = args
    for (xmlFile <- allTimeNormFiles(new File(dir))) {
      val textPath = xmlFile.getPath.replaceAll("[.][^.]*[.][^.]*[.][^.]*.xml", "")
      println(xmlFile)
      println(textPath)
      implicit val data = Data.fromPaths(xmlFile.getPath, textPath)
      for (entity <- data.entities.sortBy(_.fullSpan); if entity.`type` != "NotNormalizable") {
        printf("\"%s\" %s\n", entity.text, AnaforaReader.temporal(entity))
      }
    }
  }

  def allTimeNormFiles(dir: File): Array[File] = {
    val files = dir.listFiles()
    val xmlFiles = files.filter(_.getName.matches(".*[.]TimeNorm[.](?!preannotation)[^.]*[.][^.]*.xml"))
    val subFiles = files.filter(_.isDirectory).flatMap(allTimeNormFiles)
    xmlFiles ++ subFiles
  }

}
