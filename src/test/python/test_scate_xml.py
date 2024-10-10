import datetime

import scate
import inspect
import xml.etree.ElementTree as ET


def _isoformats(objects: list[scate.Interval | scate.Offset]):
    result = []
    for o in objects:
        match o:
            case scate.Offset():
                result.append(None)
            case scate.Interval():
                result.append(o.isoformat())
            case scate.Intervals():
                result.append(list(o.isoformats()))
            case other:
                raise NotImplementedError(other)
    return result


def test_period_calendar_interval():
    for entity_type, prop_type, number_id, obj in [
            ("Calendar-Interval", "Quarter-Year", '',
             scate.Repeating(scate.QUARTER_YEAR, span=(48, 52))),
            ("Period", "Quarter-Years", '160@e@sample-3-982-540-2@gold',
             scate.Period(scate.QUARTER_YEAR, 59, span=(45, 52))),
            ("Period", "Years", '160@e@sample-3-982-540-2@gold',
             scate.Period(scate.YEAR, 59, span=(45, 52))),
            ("Period", "Centuries", '160@e@sample-3-982-540-2@gold',
             scate.Period(scate.CENTURY, 59, span=(45, 52))),
            ("Period", "Unknown", '160@e@sample-3-982-540-2@gold',
             scate.Period(None, 59, span=(45, 52))),
            ("Period", "Years", '',
             scate.Period(scate.YEAR, None, span=(48, 52))),
            ("Period", "Unknown", '',
             scate.Period(None, None, span=(48, 52)))]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>160@e@sample-3-982-540-2@gold</id>
                        <span>45,47</span>
                        <type>Number</type>
                        <parentsType>Other</parentsType>
                        <properties>
                            <Value>59</Value>
                        </properties>
                    </entity>
                    <entity>
                        <id>161@e@sample-3-982-540-2@gold</id>
                        <span>48,52</span>
                        <type>{entity_type}</type>
                        <parentsType>Duration</parentsType>
                        <properties>
                            <Type>{prop_type}</Type>
                            <Number>{number_id}</Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        objects = scate.from_xml(ET.fromstring(xml_str))
        assert objects == [obj]
        assert _isoformats(objects) == [None]


def test_sum():
    xml_str = inspect.cleandoc(f"""
        <data>
            <annotations>
                <entity>
                    <id>115@e@WSJ_20130321_1145@gold</id>
                    <span>1578,1579</span>
                    <type>Number</type>
                    <parentsType>Other</parentsType>
                    <properties>
                        <Value>3</Value>
                    </properties>
                </entity>
                <entity>
                    <id>116@e@WSJ_20130321_1145@gold</id>
                    <span>1580,1582</span>
                    <type>Number</type>
                    <parentsType>Other</parentsType>
                    <properties>
                        <Value>7</Value>
                    </properties>
                </entity>
                <entity>
                    <id>117@e@WSJ_20130321_1145@gold</id>
                    <span>1583,1585</span>
                    <type>Number</type>
                    <parentsType>Other</parentsType>
                    <properties>
                        <Value>35</Value>
                    </properties>
                </entity>
                <entity>
                    <id>118@e@WSJ_20130321_1145@gold</id>
                    <span>1578,1579</span>
                    <type>Period</type>
                    <parentsType>Duration</parentsType>
                    <properties>
                        <Type>Hours</Type>
                        <Number>115@e@WSJ_20130321_1145@gold</Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>119@e@WSJ_20130321_1145@gold</id>
                    <span>1580,1582</span>
                    <type>Period</type>
                    <parentsType>Duration</parentsType>
                    <properties>
                        <Type>Minutes</Type>
                        <Number>116@e@WSJ_20130321_1145@gold</Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>120@e@WSJ_20130321_1145@gold</id>
                    <span>1583,1585</span>
                    <type>Period</type>
                    <parentsType>Duration</parentsType>
                    <properties>
                        <Type>Seconds</Type>
                        <Number>117@e@WSJ_20130321_1145@gold</Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>121@e@WSJ_20130321_1145@gold</id>
                    <span>1578,1579</span>
                    <type>Sum</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Periods>118@e@WSJ_20130321_1145@gold</Periods>
                        <Periods>119@e@WSJ_20130321_1145@gold</Periods>
                        <Periods>120@e@WSJ_20130321_1145@gold</Periods>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    h3 = scate.Period(scate.HOUR, 3, span=(1578, 1579))
    m7 = scate.Period(scate.MINUTE, 7, span=(1580, 1582))
    s35 = scate.Period(scate.SECOND, 35, span=(1583, 1585))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [scate.PeriodSum([h3, m7, s35], span=(1578, 1585))]
    assert _isoformats(objects) == [None]


def test_repeating_intervals():
    for op_type, prop_xml, unit, range, value in [
            ("Month-Of-Year", "<Type>December</Type>", scate.MONTH, scate.YEAR, 12),
            ("Day-Of-Month", "<Value>11</Value>", scate.DAY, scate.MONTH, 11),
            ("Day-Of-Week", "<Type>Wednesday</Type>", scate.DAY, scate.WEEK, 2),
            ("Hour-Of-Day", "<Value>11</Value>", scate.HOUR, scate.DAY, 11),
            ("Minute-Of-Hour", "<Value>11</Value>", scate.MINUTE, scate.HOUR, 11),
            ("Second-Of-Minute", "<Value>11</Value>", scate.SECOND, scate.MINUTE, 11)]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>0@e@x@gold</id>
                        <span>123,456</span>
                        <type>{op_type}</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            {prop_xml}
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        obj = scate.Repeating(unit, range, value=value, span=(123, 456))
        objects = scate.from_xml(ET.fromstring(xml_str))
        assert objects == [obj], op_type
        assert _isoformats(objects) == [None]


def test_special_repeating():
    for xml_type, xml_name, cls in [
            ("Season-Of-Year", "Spring", scate.Spring),
            ("Season-Of-Year", "Summer", scate.Summer),
            ("Season-Of-Year", "Fall", scate.Fall),
            ("Season-Of-Year", "Winter", scate.Winter),
            ("Part-Of-Week", "Weekend", scate.Weekend),
            ("Part-Of-Day", "Morning", scate.Morning),
            ("Part-Of-Day", "Noon", scate.Noon),
            ("Part-Of-Day", "Afternoon", scate.Afternoon),
            ("Part-Of-Day", "Day", scate.Day),
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
        assert _isoformats(objects) == [None]


def test_year():
    for value, digits, n_missing_digits, iso in [
            ("1999", 1999, 0, "1999-01-01T00:00:00 2000-01-01T00:00:00"),
            ("199?", 199, 1, "1990-01-01T00:00:00 2000-01-01T00:00:00"),
            ("19??", 19, 2, "1900-01-01T00:00:00 2000-01-01T00:00:00"),
            ("1???", 1, 3, "1000-01-01T00:00:00 2000-01-01T00:00:00")]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>4@test</id>
                        <span>11,15</span>
                        <type>Year</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Value>{value}</Value>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        obj = scate.Year(digits, n_missing_digits, span=(11, 15))
        objects = scate.from_xml(ET.fromstring(xml_str))
        assert objects == [obj]
        assert _isoformats(objects) == [iso]


def test_two_digit_year():
    for value, last_digits, n_suffix_digits, n_missing_digits, iso in [
            ("84", 84, 2, 0, "1984-01-01T00:00:00 1985-01-01T00:00:00"),
            ("19", 19, 2, 0, "1919-01-01T00:00:00 1920-01-01T00:00:00"),
            ("9?", 9, 1, 1, "1990-01-01T00:00:00 2000-01-01T00:00:00")]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>5@test</id>
                        <span>1704,1706</span>
                        <type>Two-Digit-Year</type>
                        <parentsType>Operator</parentsType>
                        <properties>
                            <Interval-Type>DocTime</Interval-Type>
                            <Interval></Interval>
                            <Value>{value}</Value>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        doc_time = scate.Interval.of(1928, 2, 13)
        obj = scate.YearSuffix(doc_time, last_digits, n_suffix_digits, n_missing_digits, span=(1704, 1706))
        objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
        assert objects == [obj]
        assert _isoformats(objects) == [iso]


def test_interval_offset_operators():
    doc_time = scate.Interval.of(2024, 2)
    for xml_type, cls, interval_included, iso in [
            ("After", scate.After, False, "2025-02-01T00:00:00 2025-03-01T00:00:00"),
            ("After", scate.After, True, "2024-02-01T00:00:00 2024-03-01T00:00:00"),
            ("Before", scate.Before, False, "2023-02-01T00:00:00 2023-03-01T00:00:00"),
            ("Before", scate.Before, True, "2024-02-01T00:00:00 2024-03-01T00:00:00"),
            ("Last", scate.Last, False, "2023-02-01T00:00:00 2023-03-01T00:00:00"),
            ("Last", scate.Last, True, "2024-02-01T00:00:00 2024-03-01T00:00:00"),
            ("Next", scate.Next, False, "2025-02-01T00:00:00 2025-03-01T00:00:00"),
            ("Next", scate.Next, True, "2024-02-01T00:00:00 2024-03-01T00:00:00"),
            ("This", scate.This, None, "2024-02-01T00:00:00 2024-03-01T00:00:00")]:
        semantics = "Interval-Included" if interval_included else "Interval-Not-Included"
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>0@e@Doc9@gold</id>
                        <span>1,5</span>
                        <type>{xml_type}</type>
                        <parentsType>Operator</parentsType>
                        <properties>
                            <Semantics>{semantics}</Semantics>
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
        feb = scate.Repeating(scate.MONTH, scate.YEAR, value=2, span=(6, 14))
        kwargs = {} if interval_included is None else dict(interval_included=interval_included)
        op = cls(doc_time, feb, span=(1, 14), **kwargs)
        objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
        assert objects == [op]
        assert _isoformats(objects) == [iso], f"{xml_type}(interval_included={interval_included})"


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
        thu = scate.Repeating(scate.DAY, scate.WEEK, value=3, span=(6, 14))
        nth = scate.Nth(y2024, thu, index=3, from_end=from_end, span=(1, 19))
        objects = scate.from_xml(ET.fromstring(xml_str))
        assert objects == [nth]
        assert _isoformats(objects) == [iso]


def test_n_operators():
    y2024 = scate.Year(2024, span=(20, 25))
    mon = scate.Repeating(scate.DAY, scate.WEEK, value=0, span=(30, 35))
    kwargs = dict(n=2, interval=y2024, offset=mon, span=(10, 45))
    for xml_type, obj, isos in [
            ("NthFromEnd", scate.NthN(index=3, from_end=True, **kwargs), [[
                "2024-12-02T00:00:00 2024-12-03T00:00:00",  # 5th from last Monday in 2024
                "2024-11-25T00:00:00 2024-11-26T00:00:00",  # 6th from last Monday in 2024
            ]]),
            ("NthFromStart", scate.NthN(index=3, from_end=False, **kwargs), [[
                "2024-01-29T00:00:00 2024-01-30T00:00:00",  # 5th Monday in 2024
                "2024-02-05T00:00:00 2024-02-06T00:00:00",  # 6th Monday in 2024
            ]]),
            ("Last", scate.LastN(**kwargs), [[
                "2023-12-25T00:00:00 2023-12-26T00:00:00",  # last Monday in 2023
                "2023-12-18T00:00:00 2023-12-19T00:00:00",  # 2nd to last Monday in 2023
            ]]),
            ("Next", scate.NextN(**kwargs), [[
                "2025-01-06T00:00:00 2025-01-07T00:00:00",  # 1st Monday in 2025
                "2025-01-13T00:00:00 2025-01-14T00:00:00",  # 2nd Monday in 2025
            ]]),
            ("Before", scate.Before(**kwargs), [
                "2023-12-18T00:00:00 2023-12-19T00:00:00",  # 2nd to last Monday in 2023
            ]),
            ("After", scate.After(**kwargs), [
                "2025-01-13T00:00:00 2025-01-14T00:00:00",  # 2nd Monday in 2025
            ]),
    ]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>0@e@Doc9@gold</id>
                        <span>10,15</span>
                        <type>{xml_type}</type>
                        <parentsType>Operator</parentsType>
                        <properties>
                            <Interval-Type>Link</Interval-Type>
                            <Interval>3@e@Doc9@gold</Interval>
                            <Value>3</Value>
                            <Semantics>Interval-Not-Included</Semantics>
                            <Period></Period>
                            <Repeating-Interval>2@e@Doc9@gold</Repeating-Interval>
                        </properties>
                    </entity>
                    <entity>
                        <id>3@e@Doc9@gold</id>
                        <span>20,25</span>
                        <type>Year</type>
                        <parentsType>Interval</parentsType>
                        <properties>
                            <Value>2024</Value>
                            <Sub-Interval></Sub-Interval>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                    <entity>
                        <id>2@e@Doc9@gold</id>
                        <span>30,35</span>
                        <type>Day-Of-Week</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Type>Monday</Type>
                            <Number>1@e@Doc9@gold</Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                    <entity>
                        <id>1@e@Doc9@gold</id>
                        <span>40,45</span>
                        <type>Number</type>
                        <parentsType>Other</parentsType>
                        <properties>
                            <Value>2</Value>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        objects = scate.from_xml(ET.fromstring(xml_str))
        assert objects == [obj], xml_type
        assert _isoformats(objects) == isos, xml_type


def test_sub_interval():
    xml_str = inspect.cleandoc(f"""
        <data>
            <annotations>
                <entity>
                    <id>221@e@sample-3-982-540-2@gold</id>
                    <span>6505,6507</span>
                    <type>Day-Of-Month</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Value>28</Value>
                        <Sub-Interval>224@e@sample-3-982-540-2@gold</Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>222@e@sample-3-982-540-2@gold</id>
                    <span>6508,6511</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>May</Type>
                        <Sub-Interval>221@e@sample-3-982-540-2@gold</Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>223@e@sample-3-982-540-2@gold</id>
                    <span>6512,6516</span>
                    <type>Year</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>2000</Value>
                        <Sub-Interval>222@e@sample-3-982-540-2@gold</Sub-Interval>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>224@e@sample-3-982-540-2@gold</id>
                    <span>6517,6519</span>
                    <type>Hour-Of-Day</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Value>15</Value>
                        <AMPM-Of-Day></AMPM-Of-Day>
                        <Time-Zone></Time-Zone>
                        <Sub-Interval>225@e@sample-3-982-540-2@gold</Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>225@e@sample-3-982-540-2@gold</id>
                    <span>6520,6522</span>
                    <type>Minute-Of-Hour</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Value>6</Value>
                        <Sub-Interval></Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert _isoformats(objects) == ["2000-05-28T15:06:00 2000-05-28T15:07:00"]


def test_am_pm():
    for hour_value, am_pm, hour in [
            ("12", "AM", 0),
            ("7", "AM", 7),
            ("12", "PM", 12),
            ("5", "PM", 17)]:
        xml_str = inspect.cleandoc(f"""
            <data>
                <annotations>
                    <entity>
                        <id>794@e@doc0002_CLIN@gold</id>
                        <span>1128,1129</span>
                        <type>Hour-Of-Day</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Value>{hour_value}</Value>
                            <AMPM-Of-Day>796@e@doc0002_CLIN@gold</AMPM-Of-Day>
                            <Time-Zone></Time-Zone>
                            <Sub-Interval>795@e@doc0002_CLIN@gold</Sub-Interval>
                            <Number></Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                    <entity>
                        <id>795@e@doc0002_CLIN@gold</id>
                        <span>1130,1132</span>
                        <type>Minute-Of-Hour</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Value>30</Value>
                            <Sub-Interval></Sub-Interval>
                            <Number></Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                    <entity>
                        <id>796@e@doc0002_CLIN@gold</id>
                        <span>1133,1137</span>
                        <type>AMPM-Of-Day</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Type>{am_pm}</Type>
                            <Number></Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        h = scate.Repeating(scate.HOUR, scate.DAY, value=hour, span=(1128, 1129))
        m30 = scate.Repeating(scate.MINUTE, scate.HOUR, value=30, span=(1130, 1132))
        hm30 = scate.RepeatingIntersection([h, m30], span=(1128, 1137))
        assert scate.from_xml(ET.fromstring(xml_str)) == [hm30]


def test_doc_time():
    doc_time = scate.Interval.of(2024, 2, 2)
    doc_time_year = scate.Year(2024)
    for xml_type, xml_interval_type, cls, cls_doc_time, iso in [
            ("Before", "DocTime", scate.Before, doc_time, "2024-01-15T00:00:00 2024-01-16T00:00:00"),
            ("After", "DocTime-Year", scate.After, doc_time_year, "2025-01-15T00:00:00 2025-01-16T00:00:00"),
            ("Last", "DocTime-Year", scate.Last, doc_time_year, "2023-12-15T00:00:00 2023-12-16T00:00:00"),
            ("Next", "DocTime", scate.Next, doc_time, "2024-02-15T00:00:00 2024-02-16T00:00:00"),
            ("This", "DocTime", scate.This, doc_time, "2024-02-15T00:00:00 2024-02-16T00:00:00"),
            ("NthFromEnd", "DocTime-Year", scate.Nth, doc_time_year, "2024-12-15T00:00:00 2024-12-16T00:00:00"),
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
                            <Semantics>Interval-Not-Included</Semantics>
                            <Interval-Type>{xml_interval_type}</Interval-Type>
                            <Interval></Interval>
                            <Value>1</Value>
                            <Period></Period>
                            <Repeating-Interval>1@e@Doc9@gold</Repeating-Interval>
                        </properties>
                    </entity>
                    <entity>
                        <id>1@e@Doc9@gold</id>
                        <span>6,14</span>
                        <type>Day-Of-Month</type>
                        <parentsType>Repeating-Interval</parentsType>
                        <properties>
                            <Value>15</Value>
                            <Number></Number>
                            <Modifier></Modifier>
                        </properties>
                    </entity>
                </annotations>
            </data>""")
        kwargs = dict(index=1, from_end=True) if cls == scate.Nth else {}
        d15 = scate.Repeating(scate.DAY, scate.MONTH, value=15, span=(6, 14))
        op = cls(cls_doc_time, d15, span=(1, 14), **kwargs)
        objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
        assert objects == [op]
        assert _isoformats(objects) == [iso]


def test_discontinuous_span():
    xml_str = inspect.cleandoc(f"""
        <data>
            <annotations>
                <entity>
                    <id>177@e@Food_Assistance_Outlook_Brief_1-Jan-18@gold</id>
                    <span>3605,3615;3633,3639</span>
                    <type>Season-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>Unknown</Type>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [scate.Repeating(None, span=(3605, 3639))]
    assert _isoformats(objects) == [None]


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
    d25noon = scate.RepeatingIntersection([d25, noon], span=(8, 15))
    m11d25noon = scate.RepeatingIntersection([m11, d25noon], span=(5, 15))
    date = scate.This(y2000, m11d25noon, span=(0, 15))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [date]
    assert _isoformats(objects) == ["2000-10-25T12:00:00 2000-10-25T12:01:00"]


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
    assert _isoformats(objects) == ["2000-10-25T12:00:00 2000-10-25T12:01:00"]


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
    assert objects == [ref_after_dec_2017]
    assert _isoformats(objects) == ["2018-01-01T00:00:00 ..."]


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
    m12d25 = scate.RepeatingIntersection([m12, d25], span=(5, 16))
    op = scate.Last(doc_time, m12d25, span=(0, 16))
    objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
    assert objects == [op]
    assert _isoformats(objects) == ["2017-12-25T00:00:00 2017-12-26T00:00:00"]


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
    m12d25 = scate.RepeatingIntersection([m12, d25], span=(5, 16))
    op = scate.This(doc_time, m12d25, span=(0, 16))
    objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
    assert objects == [op]
    assert _isoformats(objects) == ["2018-12-25T00:00:00 2018-12-26T00:00:00"]


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
    m12d25 = scate.RepeatingIntersection([m11, d25], span=(0, 11))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [m12d25]
    assert _isoformats(objects) == [None]


def test_december_2017_to_january_2018():
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>1@e@Doc9@gold</id>
                    <span>0,8</span>
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
                    <span>9,13</span>
                    <type>Year</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>2017</Value>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>3@e@Doc9@gold</id>
                    <span>14,16</span>
                    <type>Between</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Start-Interval-Type>Link</Start-Interval-Type>
                        <Start-Interval>1@e@Doc9@gold</Start-Interval>
                        <Start-Included>Included</Start-Included>
                        <End-Interval-Type>Link</End-Interval-Type>
                        <End-Interval>4@e@Doc9@gold</End-Interval>
                        <End-Included>Included</End-Included>
                    </properties>
                </entity>
                <entity>
                    <id>4@e@Doc9@gold</id>
                    <span>17,24</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>January</Type>
                        <Number></Number>
                        <Modifier></Modifier>
                        <Super-Interval>5@e@Doc9@gold</Super-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>5@e@Doc9@gold</id>
                    <span>25,29</span>
                    <type>Year</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>2018</Value>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    m12 = scate.Repeating(scate.MONTH, scate.YEAR, value=12, span=(0, 8))
    y2017 = scate.Year(2017, span=(9, 13))
    m12y2017 = scate.This(y2017, m12, span=(0, 13))
    m01 = scate.Repeating(scate.MONTH, scate.YEAR, value=1, span=(17, 24))
    y2018 = scate.Year(2018, span=(25, 29))
    m01y2018 = scate.This(y2018, m01, span=(17, 29))
    between = scate.Between(m12y2017, m01y2018, start_included=True, end_included=True, span=(0, 29))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [between]
    assert _isoformats(objects) == ["2017-12-01T00:00:00 2018-02-01T00:00:00"]


def test_december_17_and_18():
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>1@e@Doc9@gold</id>
                    <span>0,8</span>
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
                    <span>9,11</span>
                    <type>Day-Of-Month</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>17</Value>
                        <Modifier></Modifier>
                        <Super-Interval>1@e@Doc9@gold</Super-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>3@e@Doc9@gold</id>
                    <span>16,18</span>
                    <type>Day-Of-Month</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>18</Value>
                        <Modifier></Modifier>
                        <Super-Interval>1@e@Doc9@gold</Super-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>4@e@Doc8@gold</id>
                    <span>12,15</span>
                    <type>Union</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Repeating-Intervals>2@e@Doc9@gold</Repeating-Intervals>
                        <Repeating-Intervals>3@e@Doc9@gold</Repeating-Intervals>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    m12 = scate.Repeating(scate.MONTH, scate.YEAR, value=12, span=(0, 8))
    d17 = scate.Repeating(scate.DAY, scate.MONTH, value=17, span=(9, 11))
    d17m12 = scate.RepeatingIntersection([m12, d17], span=(0, 11))
    d18 = scate.Repeating(scate.DAY, scate.MONTH, value=18, span=(16, 18))
    d18m12 = scate.RepeatingIntersection([m12, d18], span=(0, 18))
    union = scate.OffsetUnion([d17m12, d18m12], span=(0, 18))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [union]
    assert _isoformats(objects) == [None]
    [obj_union] = objects
    assert (scate.Year(2024) + obj_union).isoformat() == \
           "2025-12-17T00:00:00 2025-12-18T00:00:00"
    assert (scate.Year(2024) + obj_union + obj_union).isoformat() == \
           "2025-12-18T00:00:00 2025-12-19T00:00:00"
    assert (scate.Year(2024) + obj_union + obj_union + obj_union).isoformat() == \
           "2026-12-17T00:00:00 2026-12-18T00:00:00"
    assert (scate.Year(2024) - obj_union).isoformat() == \
           "2023-12-18T00:00:00 2023-12-19T00:00:00"
    assert (scate.Year(2024) - obj_union - obj_union).isoformat() == \
           "2023-12-17T00:00:00 2023-12-18T00:00:00"
    assert (scate.Year(2024) - obj_union - obj_union - obj_union).isoformat() == \
           "2022-12-18T00:00:00 2022-12-19T00:00:00"


def test_first_nine_months_of_1997():
    # NYT19980206.0460 (2979,3004) first nine months of 1997
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>206@e@NYT19980206.0460@gold</id>
                    <span>0,5</span>
                    <type>NthFromStart</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Interval-Type>Link</Interval-Type>
                        <Interval>209@e@NYT19980206.0460@gold</Interval>
                        <Value>1</Value>
                        <Period></Period>
                        <Repeating-Interval>208@e@NYT19980206.0460@gold</Repeating-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>207@e@NYT19980206.0460@gold</id>
                    <span>6,10</span>
                    <type>Number</type>
                    <parentsType>Other</parentsType>
                    <properties>
                        <Value>9</Value>
                    </properties>
                </entity>
                <entity>
                    <id>208@e@NYT19980206.0460@gold</id>
                    <span>11,17</span>
                    <type>Calendar-Interval</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>Month</Type>
                        <Number>207@e@NYT19980206.0460@gold</Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>209@e@NYT19980206.0460@gold</id>
                    <span>21,25</span>
                    <type>Year</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>1997</Value>
                        <Sub-Interval></Sub-Interval>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    month = scate.Repeating(scate.MONTH, span=(11, 17))
    y1997 = scate.Year(1997, span=(21, 25))
    nth = scate.NthN(y1997, month, index=1, n=9, from_end=False, span=(0, 25))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [nth]
    assert _isoformats(objects) == [[scate.Interval.of(1997, i).isoformat() for i in range(1, 10)]]


def test_last_few_months():
    # NYT19980206.0460 (3441,3445) last few months
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>210@e@NYT19980206.0460@gold</id>
                    <span>0,4</span>
                    <type>Last</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Semantics>Standard</Semantics>
                        <Interval-Type>DocTime</Interval-Type>
                        <Interval></Interval>
                        <Period></Period>
                        <Repeating-Interval>212@e@NYT19980206.0460@gold</Repeating-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>211@e@NYT19980206.0460@gold</id>
                    <span>5,8</span>
                    <type>Number</type>
                    <parentsType>Other</parentsType>
                    <properties>
                        <Value>?</Value>
                    </properties>
                </entity>
                <entity>
                    <id>212@e@NYT19980206.0460@gold</id>
                    <span>9,15</span>
                    <type>Calendar-Interval</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>Month</Type>
                        <Number>211@e@NYT19980206.0460@gold</Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    doc_time = scate.Interval.of(1998, 2, 6)
    month = scate.Repeating(scate.MONTH, span=(9, 15))
    last_n = scate.LastN(doc_time, month, n=None, span=(0, 15))
    objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
    assert objects == [last_n]
    assert _isoformats(objects) == [["1998-01-01T00:00:00 1998-02-01T00:00:00", "... 1998-01-01T00:00:00"]]


def test_19980331():
    # VOA19980331.1700.1533 (25,29) 19980331
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>70@e@VOA19980331.1700.1533@gold</id>
                    <span>0,4</span>
                    <type>Year</type>
                    <parentsType>Interval</parentsType>
                    <properties>
                        <Value>1998</Value>
                        <Sub-Interval>65@e@VOA19980331.1700.1533@gold</Sub-Interval>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>65@e@VOA19980331.1700.1533@gold</id>
                    <span>4,6</span>
                    <type>Month-Of-Year</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>March</Type>
                        <Sub-Interval>61@e@VOA19980331.1700.1533@gold</Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>61@e@VOA19980331.1700.1533@gold</id>
                    <span>6,8</span>
                    <type>Day-Of-Month</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Value>31</Value>
                        <Sub-Interval></Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    y1998 = scate.Year(1998, span=(0, 4))
    m3 = scate.Repeating(scate.MONTH, scate.YEAR, value=3, span=(4, 6))
    d31 = scate.Repeating(scate.DAY, scate.MONTH, value=31, span=(6, 8))
    m3d31 = scate.RepeatingIntersection([m3, d31], span=(4, 8))
    y1998m3d31 = scate.This(y1998, m3d31, span=(0, 8))
    objects = scate.from_xml(ET.fromstring(xml_str))
    assert objects == [y1998m3d31]
    assert _isoformats(objects) == ["1998-03-31T00:00:00 1998-04-01T00:00:00"]


def test_friday():
    # APW19980306.1001 (1705,1711) Friday
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>117@e@APW19980306.1001@gold</id>
                    <span>0,6</span>
                    <type>Day-Of-Week</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>Friday</Type>
                        <Sub-Interval></Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>118@e@APW19980306.1001@gold</id>
                    <span>0,6</span>
                    <type>Last</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Semantics>Interval-Included</Semantics>
                        <Interval-Type>DocTime</Interval-Type>
                        <Interval></Interval>
                        <Period></Period>
                        <Repeating-Interval>117@e@APW19980306.1001@gold</Repeating-Interval>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    doc_time = scate.Interval.of(1998, 3, 6)
    fri = scate.Repeating(scate.DAY, scate.WEEK, value=4, span=(0, 6))
    last = scate.Last(doc_time, fri, interval_included=True, span=(0, 6))
    objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
    assert objects == [last]
    assert _isoformats(objects) == [doc_time.isoformat()]


def test_earlier_sunday():
    # APW19980322.0749 (3918,3925) Earlier Sunday
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>125@e@APW19980322.0749@gold</id>
                    <span>3926,3932</span>
                    <type>Day-Of-Week</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>Sunday</Type>
                        <Sub-Interval></Sub-Interval>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
                <entity>
                    <id>177@e@APW19980322.0749@gold</id>
                    <span>3926,3932</span>
                    <type>Last</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Semantics>Interval-Included</Semantics>
                        <Interval-Type>DocTime</Interval-Type>
                        <Interval></Interval>
                        <Period></Period>
                        <Repeating-Interval>125@e@APW19980322.0749@gold</Repeating-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>178@e@APW19980322.0749@gold</id>
                    <span>3918,3925</span>
                    <type>Before</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Interval-Type>Link</Interval-Type>
                        <Interval>179@e@APW19980322.0749@gold</Interval>
                        <Period></Period>
                        <Repeating-Interval></Repeating-Interval>
                        <Semantics>Interval-Not-Included</Semantics>
                    </properties>
                </entity>
                <entity>
                    <id>179@e@APW19980322.0749@gold</id>
                    <span>3750,3759</span>
                    <type>Event</type>
                    <parentsType>Other</parentsType>
                    <properties>
                    </properties>
                </entity>
                <entity>
                    <id>180@e@APW19980322.0749@gold</id>
                    <span>3918,3925</span>
                    <type>Intersection</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Intervals>178@e@APW19980322.0749@gold</Intervals>
                        <Intervals>177@e@APW19980322.0749@gold</Intervals>
                        <Repeating-Intervals></Repeating-Intervals>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    doc_time = scate.Interval.of(1998, 3, 22)
    sun = scate.Repeating(scate.DAY, scate.WEEK, value=6, span=(3926, 3932))
    last = scate.Last(doc_time, sun, interval_included=True, span=(3926, 3932))
    event = scate.Interval(None, None)
    before = scate.Before(event, None, span=(3918, 3925))
    intersection = scate.Intersection([before, last], span=(3918, 3932))
    objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
    assert objects == [intersection]
    assert _isoformats(objects) == ["... ..."]

    # test again with event interval specified
    event = scate.Interval.fromisoformat("1998-03-22T12:00:00 1998-03-23T12:00:00")
    before = scate.Before(event, None, span=(3918, 3925))
    intersection = scate.Intersection([before, last], span=(3918, 3932))
    objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={
        (None, None): doc_time,
        (3750, 3759): event,
    })
    assert objects == [intersection]
    assert _isoformats(objects) == ["1998-03-22T00:00:00 1998-03-22T12:00:00"]


def test_20th_century():
    # bbc_20130322_1150 (1969,1981) 20th Century
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>93@e@bbc_20130322_1150@gold</id>
                    <span>1969,1973</span>
                    <type>NthFromStart</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Interval-Type>DocTime-Era</Interval-Type>
                        <Interval></Interval>
                        <Value>20</Value>
                        <Period></Period>
                        <Repeating-Interval>94@e@bbc_20130322_1150@gold</Repeating-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>94@e@bbc_20130322_1150@gold</id>
                    <span>1974,1981</span>
                    <type>Calendar-Interval</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>Century</Type>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    doc_time = scate.Interval.of(2013, 3, 22)
    era_time = scate.Interval(datetime.datetime.min, None)
    century = scate.Repeating(scate.CENTURY, span=(1974, 1981))
    nth = scate.Nth(era_time, century, index=20, span=(1969, 1981))
    objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
    assert objects == [nth]
    assert _isoformats(objects) == ["1900-01-01T00:00:00 2000-01-01T00:00:00"]


def test_every_other_day():
    # ID176_clinic_416 (6149, 6154) Every-other-day
    xml_str = inspect.cleandoc("""
        <data>
            <annotations>
                <entity>
                    <id>729@e@ID176_clinic_416@gold</id>
                    <span>6149,6154</span>
                    <type>Every-Nth</type>
                    <parentsType>Operator</parentsType>
                    <properties>
                        <Value>2</Value>
                        <Repeating-Interval>730@e@ID176_clinic_416@gold</Repeating-Interval>
                    </properties>
                </entity>
                <entity>
                    <id>730@e@ID176_clinic_416@gold</id>
                    <span>6155,6158</span>
                    <type>Calendar-Interval</type>
                    <parentsType>Repeating-Interval</parentsType>
                    <properties>
                        <Type>Day</Type>
                        <Number></Number>
                        <Modifier></Modifier>
                    </properties>
                </entity>
            </annotations>
        </data>""")
    doc_time = scate.Interval.of(2010, 8, 5)
    day = scate.Repeating(scate.DAY, span=(6155, 6158))
    every_other_day = scate.EveryNth(day, 2, span=(6149, 6158))
    objects = scate.from_xml(ET.fromstring(xml_str), known_intervals={(None, None): doc_time})
    assert objects == [every_other_day]
    assert _isoformats(objects) == [None]
