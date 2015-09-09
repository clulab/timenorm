package info.bethard.anafora

import com.codecommit.antixml.Elem
import com.codecommit.antixml.text


object Data {
  def apply(xml: Elem) = new Data(xml)
}
class Data(xml: Elem) {
  lazy val entities: IndexedSeq[Entity] = xml \ "annotations" \ "entity" map Entity.apply
  lazy val relations: IndexedSeq[Relation] = xml \ "annotations" \ "relation" map Relation.apply
  private[anafora] lazy val idToEntity: Map[String, Entity] = this.entities.map(e => e.id -> e)(scala.collection.breakOut)
  private[anafora] lazy val idToRelation: Map[String, Relation] = this.relations.map(r => r.id -> r)(scala.collection.breakOut)
}


abstract class Annotation(xml: Elem) {
  lazy val Seq(id: String) = xml \ "id" \ text
  lazy val `type`: String = {
    val Seq(tpe) = xml \ "type" \ text
    tpe
  }
  lazy val properties: Properties = xml \ "properties" match {
    case Seq(elem) => Properties(elem)
  }
}

object Entity {
  def apply(xml: Elem) = new Entity(xml)
}
class Entity(xml: Elem) extends Annotation(xml) {
  lazy val spans: Set[(Int, Int)] =
    (xml \ "span" \ text).flatMap(_.split(";")).map(_.split(",").map(_.toInt) match {
      case Array(start, end) => (start, end)
    }).toSet
}

object Relation {
  def apply(xml: Elem) = new Relation(xml)
}
class Relation(xml: Elem) extends Annotation(xml)

object Properties {
  def apply(xml: Elem) = new Properties(xml)
}
class Properties(xml: Elem) {
  def apply(name: String): String = xml \ name \ text match {
    case Seq(value) => value
  }
  def entity(name: String)(implicit data: Data): Entity = data.idToEntity(this.apply(name))
  def relation(name: String)(implicit data: Data): Relation = data.idToRelation(this.apply(name))
}
