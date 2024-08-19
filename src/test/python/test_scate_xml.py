import scate
import inspect
import xml.etree.ElementTree as ET


def test_noon():
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>1@test</id>
                    <span>0,4</span>
                    <type>Year</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Value>2000</Value>
                        <Sub-Interval>2@test</Sub-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>2@test</id>
                    <span>5,7</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>October</Type>
                        <Sub-Interval>3@test</Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>3@test</id>
                    <span>8,10</span>
                    <type>Day-Of-Month</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Value>25</Value>
                        <Sub-Interval>4@test</Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>4@test</id>
                    <span>11,15</span>
                    <type>Part-Of-Day</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>Noon</Type>
                        <Sub-Interval></Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    y2000 = scate.Year(2000)
    m11 = scate.Repeating(scate.Unit.MONTH, scate.Unit.YEAR, value=10)
    d25 = scate.Repeating(scate.Unit.DAY, scate.Unit.MONTH, value=25)
    date = scate.This(y2000, scate.Intersection([m11, d25, scate.NOON]))
    [obj] = scate.from_xml(ET.fromstring(xml_str))
    assert obj == date
    assert obj.span == (0, 15)
    assert obj.interval.span == (0, 4)
    assert obj.offset.span == (5, 15)
    assert [x.span for x in obj.offset.offsets] == [(5, 7), (8, 10), (11, 15)]
