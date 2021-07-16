package org.clulab.timenorm.scate

import scala.util.control.Exception
import org.clulab.anafora.{Anafora, Data, Entity}

import java.nio.file.Paths
import util.control.Breaks._

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

        val map1: Map[Entity, Entity] = (data1.entities zip data2.entities).toMap
        val map2: Map[Entity, Entity] = (data2.entities zip data1.entities).toMap
        for ((entity1, entity2) <- data1.entities zip data2.entities) {
          assert(entity1.`type` == entity2.`type`,
            s"entity type mismatch in:\n$xmlPath1\n${entity1.xml}\n$xmlPath2\n${entity2.xml}")

          //var realEntity2: Entity = entity1
          //var realEntity2: Entity = entity2
          println("current Entity1 =" + entity1.id)
          println("current Entity2 =" + entity2.id)

          val realEntity2 = if (entity1.properties.getEntity("Sub-Interval")(data1).nonEmpty) {
            println("Sub-Interval here ! --->" + entity1.id)
            var curr = entity1
            while (curr.properties.getEntity("Sub-Interval")(data1).nonEmpty) {
              curr = curr.properties.getEntity("Sub-Interval")(data1).get
              println(curr.id)
            }
            map1(curr)
            // realEntity1 = map2(realEntity2)
          }else {
            entity2
          }

          val realEntity1 = if (entity2.properties.getEntity("Super-Interval")(data2).nonEmpty) {

            println("Super-Interval here ! --->" + entity2.id)
            var curr = entity2
            while (curr.properties.getEntity("Super-Interval")(data2).nonEmpty) {
              curr = curr.properties.getEntity("Super-Interval")(data2).get
              println(curr.id)
            }
            map2(curr)

          }else {
            entity1
          }

          println()
          println("realEntity1 = " + realEntity1.id)
          println("realEntity2 = " + realEntity2.id)

          val temporal1 = catcher.toEither(reader1.temporal(realEntity1)(data1))
          val temporal2 = catcher.toEither(reader2.temporal(realEntity2)(data2))
          println(temporal1)
          println(temporal2)
          assert(temporal1 == temporal2,
            s"temporal mismatch in:\n$xmlPath1\n${temporal1}\n$xmlPath2\n${temporal2}")

          println("Done for this entity!\n\n")
        }

      }
  }
}
