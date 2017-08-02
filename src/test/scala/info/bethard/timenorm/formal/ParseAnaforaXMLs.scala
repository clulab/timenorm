package info.bethard.timenorm.formal

import java.io.File
import java.time.LocalDateTime

import info.bethard.anafora.Data

object ParseAnaforaXMLs {

  // annotations that don't represent normalizable time expressions
  val skip = Set("NotNormalizable", "Frequency", "PreAnnotation")

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

  def main(args: Array[String]): Unit = {
    val Array(dir) = args
    for (xmlFile <- allTimeNormFiles(new File(dir))) {
      val textPath = xmlFile.getPath.replaceAll("[.][^.]*[.][^.]*[.][^.]*.xml", "")
      println(xmlFile)
      implicit val data = Data.fromPaths(xmlFile.getPath, textPath)
      val dctString = LocalDateTime.now().toString
      val dct = parseDCT(dctString)
      val aReader = new AnaforaReader(dct)
      for (entity <- data.entities.sortBy(_.fullSpan); if !skip.contains(entity.`type`)) {
        printf("\"%s\"[%s] ", entity.text, entity.spans.map(t => "%s,%s".format(t._1, t._2)).toSeq.sorted.mkString(";"))
        val temporal = aReader.temporal(entity)
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
