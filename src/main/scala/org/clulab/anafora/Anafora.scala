package org.clulab.anafora

import java.nio.file.{Files, Path, Paths}

import scala.xml.{Elem, XML}

object Data {
  def fromPaths(xmlPath: Path, textPath: Option[Path] = None): Data = {
    apply(XML.loadFile(xmlPath.toFile), textPath.map(p => new String(Files.readAllBytes(p))))
  }
  def fromPaths(xmlPath: String, textPath: Option[String]): Data = {
    fromPaths(Paths.get(xmlPath), textPath.map(Paths.get(_)))
  }
  def apply(xml: Elem, text: Option[String]) = new Data(xml, text)
}
class Data(xml: Elem, val text: Option[String]) {
  lazy val entities: IndexedSeq[Entity] = (xml \ "annotations" \ "entity" map {
    case e: Elem => Entity(e)
  }).toIndexedSeq
  lazy val relations: IndexedSeq[Relation] = (xml \ "annotations" \ "relation" map {
    case e: Elem => Relation(e)
  }).toIndexedSeq
  lazy val topEntities : IndexedSeq[Entity] = {
    val allPropertyTexts = (xml \\ "properties" flatMap { case e: Elem => e.child }).map(_.text).toSet
    this.entities.filter(x => !allPropertyTexts.contains(x.id))
  }
  private[anafora] lazy val idToEntity: Map[String, Entity] = this.entities.map(e => e.id -> e).toMap
  private[anafora] lazy val idToRelation: Map[String, Relation] = this.relations.map(r => r.id -> r).toMap
  private[anafora] lazy val idToAnnotation: Map[String, Annotation] = this.idToEntity ++ this.idToRelation
}


abstract class Annotation(val xml: Elem) {
  lazy val id: String = xml \ "id" match {
    case Seq(elem: Elem) => elem.text
  }
  lazy val `type`: String = xml \ "type" match {
    case Seq(elem: Elem) => elem.text
  }
  lazy val properties: Properties = xml \ "properties" match {
    case Seq(elem: Elem) => Properties(elem)
  }
  private def childTexts: IndexedSeq[String] = IndexedSeq.empty ++ this.properties.xml.child.collect {
    case elem: Elem => elem.text
  }

  def entityChildren(implicit data: Data): IndexedSeq[Entity] = this.childTexts.flatMap(data.idToEntity.get)

  def relationChildren(implicit data: Data): IndexedSeq[Relation] = this.childTexts.flatMap(data.idToRelation.get)

  def descendants(implicit data: Data): IndexedSeq[Annotation] = {
    this +: this.childTexts.flatMap(data.idToAnnotation.get).flatMap(_.descendants)
  }
}

object Entity {
  def apply(xml: Elem) = new Entity(xml)
}
class Entity(xml: Elem) extends Annotation(xml) {
  lazy val spans: IndexedSeq[(Int, Int)] = {
    val spans = xml \ "span" flatMap {
      case elem: Elem => elem.text.split(";").map(_.split(",").map(_.toInt) match {
        case Array(start, end) => (start, end)
      })
    }
    spans.toIndexedSeq.sorted
  }
  lazy val fullSpan: (Int, Int) = (spans.map(_._1).min, spans.map(_._2).max)
  def text(implicit data: Data): Option[String] = data.text.map(text => spans.map {
    case (start, end) => text.substring(start, end)
  }.mkString("..."))
  def expandedSpan(implicit data: Data): (Int, Int) = {
    val allSpans = this.descendants.collect{ case e: Entity => e }.flatMap(_.spans)
    (allSpans.map(_._1).min, allSpans.map(_._2).max)
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
  private def textFor(name: String): IndexedSeq[String] = (xml \ name map {
    case elem: Elem => elem.text
  }).toIndexedSeq

  def names: IndexedSeq[String] = xml.child.collect {
    case e: Elem => e.label
  }.toIndexedSeq

  def has(name: String): Boolean = this.textFor(name).nonEmpty

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

  def getEntity(name: String)(implicit data: Data): Option[Entity] = this.get(name).flatMap(data.idToEntity.get)

  def getEntities(name: String)(implicit data: Data): IndexedSeq[Entity] = this.textFor(name).flatMap(data.idToEntity.get)

  def relation(name: String)(implicit data: Data): Relation = data.idToRelation(this.apply(name))
}
