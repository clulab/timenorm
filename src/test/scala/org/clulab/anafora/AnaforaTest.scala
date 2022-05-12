package org.clulab.anafora

import org.scalatest.FunSuite

class AnaforaTest extends FunSuite {
  test("TimeML example") {
    val xml = <data>
      <annotations>
        <entity>
          <id>1@e@wsj_1073@gold</id>
          <type>TIMEX3</type>
          <span>25,30;12,20</span>
          <properties>
            <functionInDocument>CREATION_TIME</functionInDocument>
            <type>DATE</type>
            <temporalFunction>false</temporalFunction>
            <value>1989-10-25</value>
          </properties>
        </entity>
        <entity>
          <id>4@e@wsj_1073@gold</id>
          <type>EVENT</type>
          <span>332,336</span>
          <properties>
            <stem>pay</stem>
            <class>OCCURRENCE</class>
          </properties>
        </entity>
        <relation>
          <id>10@r@wsj_1073@gold</id>
          <type>MAKEINSTANCE</type>
          <properties>
            <eventID>4@e@wsj_1073@gold</eventID>
            <polarity>POS</polarity>
            <pos>VERB</pos>
            <tense>PAST</tense>
            <aspect>NONE</aspect>
          </properties>
        </relation>
        <relation>
          <id>14@r@wsj_1073@gold</id>
          <type>TLINK</type>
          <properties>
            <relatedToTime>1@e@wsj_1073@gold</relatedToTime>
            <relType>BEFORE</relType>
            <eventInstanceID>10@r@wsj_1073@gold</eventInstanceID>
            <origin>USER</origin>
          </properties>
        </relation>
      </annotations>
    </data>

    implicit val data: Data = Data(xml, Some("abcdefghijklmnopqrstuvwxyz0123456789"))
    assert(data.entities.size === 2)
    assert(data.relations.size === 2)
    val Seq(time, event) = data.entities
    val Seq(makeinstance, tlink) = data.relations

    assert(time.id === "1@e@wsj_1073@gold")
    assert(time.`type` === "TIMEX3")
    assert(time.spans === IndexedSeq((12, 20), (25, 30)))
    assert(time.fullSpan === (12, 30))
    assert(time.text === Some("mnopqrst...z0123"))
    assert(time.properties("functionInDocument") === "CREATION_TIME")
    assert(time.properties("type") === "DATE")
    assert(time.properties("temporalFunction") === "false")
    assert(time.properties("value") === "1989-10-25")
    assert(time.entityChildren === IndexedSeq.empty)
    assert(time.relationChildren === IndexedSeq.empty)
    assert(time.descendants === IndexedSeq(time))
    assert(time.properties.names === IndexedSeq("functionInDocument", "type", "temporalFunction", "value"))
    assert(time.properties.getEntity("type") === None)

    assert(event.id === "4@e@wsj_1073@gold")
    assert(event.`type` === "EVENT")
    assert(event.spans === IndexedSeq((332, 336)))
    assert(event.properties("stem") === "pay")
    assert(event.properties("class") === "OCCURRENCE")
    assert(event.entityChildren === IndexedSeq.empty)
    assert(event.relationChildren === IndexedSeq.empty)
    assert(event.descendants === IndexedSeq(event))

    assert(makeinstance.id === "10@r@wsj_1073@gold")
    assert(makeinstance.`type` === "MAKEINSTANCE")
    assert(makeinstance.properties("eventID") === "4@e@wsj_1073@gold")
    assert(makeinstance.properties.entity("eventID") === event)
    assert(makeinstance.properties("polarity") === "POS")
    assert(makeinstance.properties("pos") === "VERB")
    assert(makeinstance.properties("tense") === "PAST")
    assert(makeinstance.properties("aspect") === "NONE")
    assert(makeinstance.entityChildren === IndexedSeq(event))
    assert(makeinstance.relationChildren === IndexedSeq.empty)
    assert(makeinstance.descendants.toSet === Set(makeinstance, event))

    assert(tlink.id === "14@r@wsj_1073@gold")
    assert(tlink.`type` === "TLINK")
    assert(tlink.properties("relatedToTime") === "1@e@wsj_1073@gold")
    assert(tlink.properties.entity("relatedToTime") === time)
    assert(tlink.properties("relType") === "BEFORE")
    assert(tlink.properties("eventInstanceID") === "10@r@wsj_1073@gold")
    assert(tlink.properties.relation("eventInstanceID") === makeinstance)
    assert(tlink.properties("origin") === "USER")
    assert(tlink.entityChildren === IndexedSeq(time))
    assert(tlink.relationChildren === IndexedSeq(makeinstance))
    assert(tlink.descendants.toSet === Set(tlink, time, makeinstance, event))
  }

}
