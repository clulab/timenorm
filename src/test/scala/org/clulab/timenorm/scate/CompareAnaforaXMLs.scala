package org.clulab.timenorm.scate

import scala.util.control.Exception

import org.clulab.anafora.{Anafora, Data}

import java.nio.file.Paths

object CompareAnaforaXMLs {

  def main(args: Array[String]): Unit = args match {
    case Array(dir1, dir2) =>
      val dct = SimpleInterval.of(2010, 10, 10)
      val syntaxAndPattern = "glob:**.TimeNorm.gold.completed.xml"
      val catcher = Exception.catching(
        classOf[AnaforaReader.Exception],
        classOf[Anafora.Exception],
        classOf[UnsupportedOperationException])
      val Seq(xmlPaths1, xmlPaths2) = Seq(dir1, dir2).map {
        dir => Anafora.xmlPaths(Paths.get(dir), syntaxAndPattern).toSeq
      }
      assert(xmlPaths1.size == xmlPaths2.size,
        s"path count mismatch: ${xmlPaths1.size} != ${xmlPaths2.size}")
      for ((xmlPath1, xmlPath2) <- xmlPaths1 zip xmlPaths2) {
        // val textPath = xmlPath.getParent.resolve(xmlPath.getParent.getFileName)
        val data1 = Data.fromPaths(xmlPath1, None)
        val data2 = Data.fromPaths(xmlPath2, None)
        val reader1 = new AnaforaReader(dct)(data1)
        val reader2 = new AnaforaReader(dct)(data2)

        assert(data1.entities.size == data2.entities.size,
          s"entity count mismatch (${data1.entities.size} != ${data2.entities.size}) in\n" +
            s"$xmlPath1\n$xmlPath2")
        for ((entity1, entity2) <- data1.entities zip data2.entities) {
          assert(entity1.`type` == entity2.`type`,
            s"entity type mismatch in:\n$xmlPath1\n${entity1.xml}\n$xmlPath2\n${entity2.xml}")
          val temporal1 = catcher.opt(reader1.temporal(entity1)(data1))
          val temporal2 = catcher.opt(reader2.temporal(entity2)(data2))
          assert(temporal1 == temporal2,
            s"temporal mismatch in:\n$xmlPath1\n${temporal1}\n$xmlPath2\n${temporal2}")
        }
      }
  }
}
