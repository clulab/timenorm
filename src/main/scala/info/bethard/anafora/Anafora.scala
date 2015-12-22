package info.bethard.anafora

import com.codecommit.antixml.Elem
import com.codecommit.antixml.text
import com.codecommit.antixml.XML


object Data {
  def fromPath(path: String) = apply(XML.fromSource(io.Source.fromFile(path)))
  def apply(xml: Elem) = new Data(xml)
}
class Data(xml: Elem) {
  lazy val entities: IndexedSeq[Entity] = xml \ "annotations" \ "entity" map Entity.apply
  lazy val relations: IndexedSeq[Relation] = xml \ "annotations" \ "relation" map Relation.apply
  private[anafora] lazy val idToEntity: Map[String, Entity] = this.entities.map(e => e.id -> e)(scala.collection.breakOut)
  private[anafora] lazy val idToRelation: Map[String, Relation] = this.relations.map(r => r.id -> r)(scala.collection.breakOut)
}


abstract class Annotation(val xml: Elem) {
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
class Properties(xml: Elem) extends Annotation(xml) {
  private def textFor(name: String): IndexedSeq[String] = xml \ name \ text
  def has(name: String): Boolean = !this.textFor(name).isEmpty
  def get(name: String): Option[String] = this.textFor(name) match {
    case Seq() => None
    case Seq(value) => Some(value)
    case _ => throw new RuntimeException(s"expected 0 or 1 $name value, found $xml")
  }
  def apply(name: String): String = this.textFor(name) match {
    case Seq(value) => value
    case _ => throw new RuntimeException(s"expected single $name value, found $xml")
  }
  def entity(name: String)(implicit data: Data): Entity = data.idToEntity(this.apply(name))
  def getEntity(name: String)(implicit data: Data): Option[Entity] = this.get(name).map(data.idToEntity)
  def relation(name: String)(implicit data: Data): Relation = data.idToRelation(this.apply(name))
}
