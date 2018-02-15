package org.clulab.anafora

import com.codecommit.antixml.Elem
import com.codecommit.antixml.{text => ElemText}
import com.codecommit.antixml.XML
import com.codecommit.antixml.*

object Data {
  def fromPaths(xmlPath: String, textPath: Option[String]) = apply(
    XML.fromSource(io.Source.fromFile(xmlPath)), textPath.map(p => io.Source.fromFile(p).mkString))
  def apply(xml: Elem, text: Option[String]) = new Data(xml, text)
}
class Data(xml: Elem, val text: Option[String]) {
  lazy val entities: IndexedSeq[Entity] = xml \ "annotations" \ "entity" map Entity.apply
  lazy val relations: IndexedSeq[Relation] = xml \ "annotations" \ "relation" map Relation.apply
  lazy val topEntities : IndexedSeq[Entity] = this.entities.filter(x => !(xml \\ "properties" \ * \ ElemText contains(x.id)))
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
  lazy val spans: IndexedSeq[(Int, Int)] =
    (xml \ "span" \ ElemText).flatMap(_.split(";")).map(_.split(",").map(_.toInt) match {
      case Array(start, end) => (start, end)
    }).sorted
  lazy val fullSpan: (Int, Int) = (spans.map(_._1).min, spans.map(_._2).max)
  def text(implicit data: Data): Option[String] = data.text.map(text => spans.map {
    case (start, end) => text.substring(start, end)
  }.mkString("..."))
  def expandedSpan(implicit data: Data): (Int, Int) = {
    val allSpans = entityDescendants.flatMap(_.spans)
    (allSpans.map(_._1).min, allSpans.map(_._2).max)
  }
  def entityDescendants(implicit data: Data): IndexedSeq[Entity] = {
    val childTexts = this.properties.xml.children \ ElemText
    val childEntities = childTexts.filter(data.idToEntity.contains).map(data.idToEntity)
    IndexedSeq(this) ++ childEntities.flatMap(_.entityDescendants)
  }
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

  def getEntities(name: String)(implicit data: Data): IndexedSeq[Entity] = this.textFor(name).map(data.idToEntity)

  def relation(name: String)(implicit data: Data): Relation = data.idToRelation(this.apply(name))
}
