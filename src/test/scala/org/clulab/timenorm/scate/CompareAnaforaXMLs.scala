package org.clulab.timenorm.scate

import scala.util.control.Exception

import org.clulab.anafora.{Anafora, Data, Entity}

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
        val entityRoots1 = entityRoots(data1)
        val entityRoots2 = entityRoots(data2)
        for ((entity1, entity2) <- data1.entities zip data2.entities) {
          assert(entity1.`type` == entity2.`type`,
            s"entity type mismatch in:\n$xmlPath1\n${entity1.xml}\n$xmlPath2\n${entity2.xml}")

          val root1: Entity = entityRoots1(entity1)
          val root2: Entity = entityRoots2(entity2)

          // if both are in the middle of Sub-Interval/Super-Interval chains, they are incomparable
          if (root1 != entity1 && root2 != entity2) {
            // println(f"Skipping mid-chain:\n${entity1.xml}\n${entity2.xml}")
          } else {
            val temporal1 = catcher.withTry(reader1.temporal(root1)(data1))
            val temporal2 = catcher.withTry(reader2.temporal(root2)(data2))
            if (temporal1.toOption != temporal2.toOption) {
              println(f"Temporal mismatch in:\n$xmlPath1\n$xmlPath2\n" +
                f"${entity1.xml}\n${entity2.xml}\n${temporal1}\n${temporal2}")
            }
          }
        }
      }
  }

  def entityRoots(implicit data: Data): Map[Entity, Entity] = {
    // map children to parents
    val childTypes = Seq("Sub-Interval", "Super-Interval")
    val parents = data.entities.flatMap{ entity =>
      childTypes.flatMap(entity.properties.getEntity(_)).map(_ -> entity)
    }.toMap
    // for each entity, walk its parents to the root
    data.entities.map { entity =>
      var root = entity
      while (parents.contains(root)) {
        root = parents(root)
      }
      entity -> root
    }.toMap
  }
}
