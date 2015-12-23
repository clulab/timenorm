package info.bethard.anafora

import com.codecommit.antixml.Elem
import com.codecommit.antixml.{ text => ElemText }
import com.codecommit.antixml.XML


object Data {
  def fromPaths(xmlPath: String, textPath: String) = apply(
    XML.fromSource(io.Source.fromFile(xmlPath)),
    io.Source.fromFile(textPath).mkString)
  def apply(xml: Elem, text: String) = new Data(xml, text)
}
class Data(xml: Elem, val text: String) {
  lazy val entities: IndexedSeq[Entity] = xml \ "annotations" \ "entity" map Entity.apply
  lazy val relations: IndexedSeq[Relation] = xml \ "annotations" \ "relation" map Relation.apply
  private[anafora] lazy val idToEntity: Map[String, Entity] = this.entities.map(e => e.id -> e)(scala.collection.breakOut)
  private[anafora] lazy val idToRelation: Map[String, Relation] = this.relations.map(r => r.id -> r)(scala.collection.breakOut)
}


abstract class Annotation(val xml: Elem) {
  lazy val Seq(id: String) = xml \ "id" \ ElemText
  lazy val `type`: String = {
    val Seq(tpe) = xml \ "type" \ ElemText
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
    (xml \ "span" \ ElemText).flatMap(_.split(";")).map(_.split(",").map(_.toInt) match {
      case Array(start, end) => (start, end)
    }).toSet
  def text(implicit data: Data): String = spans.toList.sorted.map{
    case (start, end) => data.text.substring(start, end)
  }.mkString("...")
}

object Relation {
  def apply(xml: Elem) = new Relation(xml)
}
class Relation(xml: Elem) extends Annotation(xml)

object Properties {
  def apply(xml: Elem) = new Properties(xml)
}
class Properties(xml: Elem) extends Annotation(xml) {
  private def textFor(name: String): IndexedSeq[String] = xml \ name \ ElemText
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
