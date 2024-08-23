import scate
import inspect
import xml.etree.ElementTree as ET


def _isoformat(objects: list[scate.Interval | scate.Offset]):
    return [o.isoformat() if isinstance(o, scate.Interval) else None for o in objects]


def test_special_repeating():
    for xml_type, xml_name, cls in [
            ("Season-Of-Year", "Spring", scate.Spring),
            ("Season-Of-Year", "Summer", scate.Summer),
            ("Season-Of-Year", "Fall", scate.Fall),
            ("Season-Of-Year", "Winter", scate.Winter),
            ("Part-Of-Day", "Morning", scate.Morning),
            ("Part-Of-Day", "Noon", scate.Noon),
            ("Part-Of-Day", "Afternoon", scate.Afternoon),
            ("Part-Of-Day", "Evening", scate.Evening),
            ("Part-Of-Day", "Night", scate.Night)]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>4@test</id>
                        <span>11,15</span>
                        <type>{xml_type}</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Type>{xml_name}</Type>
                            <Number></Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        objects = scate.from_xml(ET.fromstring(xml_str))
        assert objects == [cls(span=(11, 15))]
        assert _isoformat(objects) == [None]


def test_interval_offset_operators():
    for xml_type, cls, iso in [
            ("After", scate.After, "2025-02-01T00:00:00 2025-03-01T00:00:00"),
            ("Before", scate.Before, "2024-02-01T00:00:00 2024-03-01T00:00:00"),
            ("Last", scate.Last, "2024-02-01T00:00:00 2024-03-01T00:00:00"),
            ("Next", scate.Next, "2025-02-01T00:00:00 2025-03-01T00:00:00"),
            ("This", scate.This, "2024-02-01T00:00:00 2024-03-01T00:00:00")]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>0@e@Doc9@gold</id>
                        <span>1,5</span>
                        <type>{xml_type}</type>
                        <parentsType>Operator</parentsType>
                        <properties>
                            <Interval-Type>DocTime</Interval-Type>
                            <Interval></Interval>
                            <Period></Period>
                            <Repeating-Interval>1@e@Doc9@gold</Repeating-Interval>
                        </properties>
                    </entity>
                    <entity>
                        <id>1@e@Doc9@gold</id>
                        <span>6,14</span>
                        <type>Month-Of-Year</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Type>February</Type>
                            <Number></Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        doc_time = scate.Interval.of(2024, 8, 23)
        feb = scate.Repeating(scate.MONTH, scate.YEAR, value=2, span=(6, 14))
        op = cls(doc_time, feb, span=(1, 14))
        objects = scate.from_xml(ET.fromstring(xml_str), doc_time)
        assert objects == [feb, op]
        assert _isoformat(objects) == [None, iso]


def test_nth_operators():
    for xml_type, from_end, iso in [
            ("NthFromEnd", True, "2024-12-12T00:00:00 2024-12-13T00:00:00"),     # 3rd from last Thursday in 2024
            ("NthFromStart", False, "2024-01-18T00:00:00 2024-01-19T00:00:00"),  # 3rd Thursday in 2024
    ]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>0@e@Doc9@gold</id>
                        <span>1,5</span>
                        <type>{xml_type}</type>
                        <parentsType>Operator</parentsType>
                        <properties>
                            <Interval-Type>Link</Interval-Type>
                            <Interval>2@e@Doc9@gold</Interval>
                            <Value>3</Value>
                            <Period></Period>
                            <Repeating-Interval>1@e@Doc9@gold</Repeating-Interval>
                        </properties>
                    </entity>
                    <entity>
                        <id>2@e@Doc9@gold</id>
                        <span>15,19</span>
                        <type>Year</type>
                        <parentsType>Interval</parentsType>
                        <properties>
                            <Value>2024</Value>
                            <Sub-Interval></Sub-Interval>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                    <entity>
                        <id>1@e@Doc9@gold</id>
                        <span>6,14</span>
                        <type>Day-Of-Week</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Type>Thursday</Type>
                            <Number></Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        y2024 = scate.Year(2024, span=(15, 19))
        thu = scate.Repeating(scate.DAY, scate.WEEK, value=scate.THURSDAY, span=(6, 14))
        nth = scate.Nth(y2024, thu, index=3, from_end=from_end, span=(1, 19))
        objects = scate.from_xml(ET.fromstring(xml_str))
        assert objects == [y2024, thu, nth]
        assert _isoformat(objects) == [y2024.isoformat(), None, iso]


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
    y2000 = scate.Year(2000, span=(0, 4))
    m11 = scate.Repeating(scate.MONTH, scate.YEAR, value=10, span=(5, 7))
    d25 = scate.Repeating(scate.DAY, scate.MONTH, value=25, span=(8, 10))
    noon = scate.Noon(span=(11, 15))
    m11d25noon = scate.Intersection([m11, d25, noon], span=(5, 15))
    date = scate.This(y2000, m11d25noon, span=(0, 15))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [date]
    assert _isoformat(objects) == ["2000-10-25T12:00:00 2000-10-25T12:01:00"]


def test_noon_super_interval():
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
                    </properties>
                </entity>
                <entity>
                    <id>2@test</id>
                    <span>5,7</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>October</Type>
                        <Super-Interval>1@test</Super-Interval>
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
                        <Super-Interval>2@test</Super-Interval>
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
                        <Super-Interval>3@test</Super-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    y2000 = scate.Year(2000, span=(0, 4))
    m11 = scate.Repeating(scate.MONTH, scate.YEAR, value=10, span=(5, 7))
    d25 = scate.Repeating(scate.DAY, scate.MONTH, value=25, span=(8, 10))
    noon = scate.Noon(span=(11, 15))
    this_y2000_m11 = scate.This(y2000, m11, span=(0, 7))
    this_y2000_m11_d25 = scate.This(this_y2000_m11, d25, span=(0, 10))
    date = scate.This(this_y2000_m11_d25, noon, span=(0, 15))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [date]
    assert _isoformat(objects) == ["2000-10-25T12:00:00 2000-10-25T12:01:00"]


def test_after_december_2017():
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>0@e@Doc9@gold</id>
                    <span>0,5</span>
                    <type>After</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Semantics>Interval-Not-Included</Semantics>
                        <!-- comment -->
                        <Interval-Type>Link</Interval-Type>
                        <Interval>1@e@Doc9@gold</Interval>
                        <Period></Period>
                        <Repeating-Interval></Repeating-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>1@e@Doc9@gold</id>
                    <span>6,14</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>December</Type>
                        <Number></Number>
                        <Modifier></Modifier>
                        <Super-Interval>2@e@Doc9@gold</Super-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>2@e@Doc9@gold</id>
                    <span>15,19</span>
                    <type>Year</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>2017</Value>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    y2017 = scate.Year(2017, span=(15, 19))
    m12 = scate.Repeating(scate.MONTH, scate.YEAR, value=12, span=(6, 14))
    ref_dec_2017 = scate.This(y2017, m12, span=(6, 19))
    ref_after_dec_2017 = scate.After(ref_dec_2017, None, span=(0, 19))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [ref_dec_2017, ref_after_dec_2017]
    assert _isoformat(objects) == [
        "2017-12-01T00:00:00 2018-01-01T00:00:00",
        "2018-01-01T00:00:00 ..."
    ]


def test_last_december_25():
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>0@e@Doc9@gold</id>
                    <span>0,4</span>
                    <type>Last</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Semantics>Interval-Not-Included</Semantics>
                        <Interval-Type>DocTime</Interval-Type>
                        <Interval></Interval>
                        <Period></Period>
                        <Repeating-Interval>2@e@Doc9@gold</Repeating-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>1@e@Doc9@gold</id>
                    <span>5,13</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>December</Type>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>2@e@Doc9@gold</id>
                    <span>14,16</span>
                    <type>Day-Of-Month</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Value>25</Value>
                        <Modifier></Modifier>
                        <Super-Interval>1@e@Doc9@gold</Super-Interval>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    doc_time = scate.Interval.of(2018, 2, 6, 22, 19)
    d25 = scate.Repeating(scate.DAY, scate.MONTH, value=25, span=(14, 16))
    m12 = scate.Repeating(scate.MONTH, scate.YEAR, value=12, span=(5, 13))
    m12d25 = scate.Intersection([m12, d25], span=(5, 16))
    op = scate.Last(doc_time, m12d25, span=(0, 16))
    objects = scate.from_xml(ET.fromstring(xml_str), doc_time)
    assert objects == [m12d25, op]
    assert _isoformat(objects) == [None, "2017-12-25T00:00:00 2017-12-26T00:00:00"]


def test_this_december_25():
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>0@e@Doc9@gold</id>
                    <span>0,4</span>
                    <type>This</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Interval-Type>DocTime</Interval-Type>
                        <Interval></Interval>
                        <Period></Period>
                        <Repeating-Interval>2@e@Doc9@gold</Repeating-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>1@e@Doc9@gold</id>
                    <span>5,13</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>December</Type>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>2@e@Doc9@gold</id>
                    <span>14,16</span>
                    <type>Day-Of-Month</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Value>25</Value>
                        <Modifier></Modifier>
                        <Super-Interval>1@e@Doc9@gold</Super-Interval>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    doc_time = scate.Interval.of(2018, 2, 6, 22, 19)
    d25 = scate.Repeating(scate.DAY, scate.MONTH, value=25, span=(14, 16))
    m12 = scate.Repeating(scate.MONTH, scate.YEAR, value=12, span=(5, 13))
    m12d25 = scate.Intersection([m12, d25], span=(5, 16))
    op = scate.This(doc_time, m12d25, span=(0, 16))
    objects = scate.from_xml(ET.fromstring(xml_str), doc_time)
    assert objects == [m12d25, op]
    assert _isoformat(objects) == [None, "2018-12-25T00:00:00 2018-12-26T00:00:00"]


def test_november_17():
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>1@e@Doc9@gold</id>
                    <span>0,8</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>November</Type>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>2@e@Doc9@gold</id>
                    <span>9,11</span>
                    <type>Day-Of-Month</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>17</Value>
                        <Modifier></Modifier>
                        <Super-Interval>1@e@Doc9@gold</Super-Interval>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    d25 = scate.Repeating(scate.DAY, scate.MONTH, value=17, span=(9, 11))
    m11 = scate.Repeating(scate.MONTH, scate.YEAR, value=11, span=(0, 8))
    m12d25 = scate.Intersection([m11, d25], span=(0, 11))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [m12d25]
    assert _isoformat(objects) == [None]
