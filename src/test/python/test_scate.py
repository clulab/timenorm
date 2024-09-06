import scate
import datetime
import pytest


def test_interval():
    assert scate.Interval.of(1985).isoformat() == "1985-01-01T00:00:00 1986-01-01T00:00:00"
    assert scate.Interval.of(1985, 6).isoformat() == "1985-06-01T00:00:00 1985-07-01T00:00:00"
    assert scate.Interval.of(1985, 6, 17).isoformat() == "1985-06-17T00:00:00 1985-06-18T00:00:00"
    assert scate.Interval.of(1985, 6, 17, 23).isoformat() == "1985-06-17T23:00:00 1985-06-18T00:00:00"
    assert scate.Interval.of(1985, 6, 17, 23, 0).isoformat() == "1985-06-17T23:00:00 1985-06-17T23:01:00"


def test_unit_truncate():
    date = datetime.datetime(2026, 5, 3, 1, 7, 35, 1111)
    assert scate.CENTURY.truncate(date).isoformat() == "2000-01-01T00:00:00"
    assert scate.QUARTER_CENTURY.truncate(date).isoformat() == "2025-01-01T00:00:00"
    assert scate.DECADE.truncate(date).isoformat() == "2020-01-01T00:00:00"
    assert scate.YEAR.truncate(date).isoformat() == "2026-01-01T00:00:00"
    assert scate.QUARTER_YEAR.truncate(date).isoformat() == "2026-04-01T00:00:00"
    assert scate.MONTH.truncate(date).isoformat() == "2026-05-01T00:00:00"
    assert scate.WEEK.truncate(date).isoformat() == "2026-04-27T00:00:00"
    assert scate.DAY.truncate(date).isoformat() == "2026-05-03T00:00:00"
    assert scate.HOUR.truncate(date).isoformat() == "2026-05-03T01:00:00"
    assert scate.MINUTE.truncate(date).isoformat() == "2026-05-03T01:07:00"
    assert scate.SECOND.truncate(date).isoformat() == "2026-05-03T01:07:35"
    assert scate.MILLISECOND.truncate(date).isoformat() == "2026-05-03T01:07:35.001000"
    assert scate.MICROSECOND.truncate(date).isoformat() == "2026-05-03T01:07:35.001111"

    date = datetime.datetime.fromisoformat("2005-01-01 00:00:00")
    assert scate.WEEK.truncate(date).isoformat() == "2004-12-27T00:00:00"


def test_period():
    date = datetime.datetime(2000, 1, 1, 0, 0, 0, 0)
    period = scate.Period(scate.YEAR, 5)
    assert (date + period).end.isoformat() == "2005-01-01T00:00:00"
    assert (date - period).start.isoformat() == "1995-01-01T00:00:00"


def test_sum():
    period1 = scate.Period(scate.YEAR, 1)
    period2 = scate.Period(scate.YEAR, 2)
    period3 = scate.Period(scate.MONTH, 3)
    period4 = scate.Period(scate.DAY, 2)
    period_sum = scate.Sum([period1, period2, period3])
    dt = datetime.datetime(2000, 6, 10, 0, 0, 0, 0)

    assert (dt + period_sum).end.isoformat() == "2003-09-10T00:00:00"
    assert (dt - period_sum).start.isoformat() == "1997-03-10T00:00:00"

    period_sum2 = scate.Sum([period4, period_sum])

    assert (dt + period_sum2).end.isoformat() == "2003-09-12T00:00:00"
    assert (dt - period_sum2).start.isoformat() == "1997-03-08T00:00:00"


def test_repeating_unit():
    century = scate.Repeating(scate.CENTURY)
    decade = scate.Repeating(scate.DECADE)
    year = scate.Repeating(scate.YEAR)
    month = scate.Repeating(scate.MONTH)
    week = scate.Repeating(scate.WEEK)
    day = scate.Repeating(scate.DAY)

    interval = scate.Interval.of(2000, 1, 1)
    assert (interval - year).isoformat() == "1999-01-01T00:00:00 2000-01-01T00:00:00"
    assert (interval - year - year).isoformat() == "1998-01-01T00:00:00 1999-01-01T00:00:00"
    assert (interval + day).isoformat() == "2000-01-02T00:00:00 2000-01-03T00:00:00"
    assert (interval + day + day).isoformat() == "2000-01-03T00:00:00 2000-01-04T00:00:00"

    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    assert (interval - month).isoformat() == scate.Interval.of(2002, 2).isoformat()
    assert (interval - month - month).isoformat() == scate.Interval.of(2002, 1).isoformat()
    assert (interval + month).isoformat() == scate.Interval.of(2003, 6).isoformat()
    assert (interval + month + month).isoformat() == scate.Interval.of(2003, 7).isoformat()
    assert (interval - century).isoformat() == "1900-01-01T00:00:00 2000-01-01T00:00:00"
    assert (interval + century).isoformat() == "2100-01-01T00:00:00 2200-01-01T00:00:00"
    assert (interval - decade).isoformat() == "1990-01-01T00:00:00 2000-01-01T00:00:00"
    assert (interval + decade).isoformat() == "2010-01-01T00:00:00 2020-01-01T00:00:00"
    # March 11, 2002 is a Monday
    assert (interval - week).isoformat() == "2002-03-11T00:00:00 2002-03-18T00:00:00"
    # May 12, 2003 is a Monday
    assert (interval + week).isoformat() == "2003-05-12T00:00:00 2003-05-19T00:00:00"

    interval = scate.Interval.fromisoformat("2001-02-12T03:03 2001-02-14T22:00")
    assert (interval - day).isoformat() == scate.Interval.of(2001, 2, 11).isoformat()
    assert (interval + day).isoformat() == scate.Interval.of(2001, 2, 15).isoformat()

    interval = scate.Interval.fromisoformat("2001-02-12 2001-02-14")
    assert (interval - day).isoformat() == scate.Interval.of(2001, 2, 11).isoformat()
    assert (interval + day).isoformat() == scate.Interval.of(2001, 2, 14).isoformat()

    # December 31, 2012 is a Monday
    interval = scate.Interval.of(2013, 1, 8)
    assert (interval - week).isoformat() == "2012-12-31T00:00:00 2013-01-07T00:00:00"

    interval = scate.Interval.of(2013, 1, 8, 12)
    days9 = scate.Repeating(scate.DAY, n_units=9)
    assert (interval + days9).isoformat() == "2013-01-09T00:00:00 2013-01-18T00:00:00"
    assert (interval - days9).isoformat() == "2012-12-30T00:00:00 2013-01-08T00:00:00"


def test_repeating_field():
    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    may = scate.Repeating(scate.MONTH, scate.YEAR, value=5)
    assert (interval - may).isoformat() == "2001-05-01T00:00:00 2001-06-01T00:00:00"
    assert (interval - may - may).isoformat() == "2000-05-01T00:00:00 2000-06-01T00:00:00"
    assert (interval + may).isoformat() == "2004-05-01T00:00:00 2004-06-01T00:00:00"
    assert (interval + may + may).isoformat() == "2005-05-01T00:00:00 2005-06-01T00:00:00"

    day29 = scate.Repeating(scate.DAY, scate.MONTH, value=29)
    assert (interval - day29).isoformat() == "2002-01-29T00:00:00 2002-01-30T00:00:00"
    assert (interval - day29 - day29).isoformat() == "2001-12-29T00:00:00 2001-12-30T00:00:00"
    assert (interval + day29).isoformat() == "2003-05-29T00:00:00 2003-05-30T00:00:00"
    assert (interval + day29 + day29).isoformat() == "2003-06-29T00:00:00 2003-06-30T00:00:00"

    # make sure that preceding and following are strict (no overlap allowed)
    nov = scate.Repeating(scate.MONTH, scate.YEAR, value=11)
    interval = scate.Interval.of(1989, 11, 2)
    assert (interval - nov).isoformat() == "1988-11-01T00:00:00 1988-12-01T00:00:00"
    assert (interval + nov).isoformat() == "1990-11-01T00:00:00 1990-12-01T00:00:00"

    day31 = scate.Repeating(scate.DAY, scate.MONTH, value=31)
    interval = scate.Interval.of(1980, 3, 1)
    assert (interval + day31).isoformat() == "1980-03-31T00:00:00 1980-04-01T00:00:00"
    interval = scate.Interval.of(2000, 2, 15)
    assert (interval + day31).isoformat() == "2000-03-31T00:00:00 2000-04-01T00:00:00"
    assert (interval - day31).isoformat() == "2000-01-31T00:00:00 2000-02-01T00:00:00"


def test_seasons():
    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    assert (interval + scate.Spring()).isoformat() == "2004-03-01T00:00:00 2004-06-01T00:00:00"
    assert (interval - scate.Spring()).isoformat() == "2001-03-01T00:00:00 2001-06-01T00:00:00"
    assert (interval + scate.Summer()).isoformat() == "2003-06-01T00:00:00 2003-09-01T00:00:00"
    assert (interval - scate.Summer()).isoformat() == "2001-06-01T00:00:00 2001-09-01T00:00:00"
    assert (interval + scate.Fall()).isoformat() == "2003-09-01T00:00:00 2003-12-01T00:00:00"
    assert (interval - scate.Fall()).isoformat() == "2001-09-01T00:00:00 2001-12-01T00:00:00"
    assert (interval + scate.Winter()).isoformat() == "2003-12-01T00:00:00 2004-03-01T00:00:00"
    assert (interval - scate.Winter()).isoformat() == "2001-12-01T00:00:00 2002-03-01T00:00:00"


def test_day_parts():
    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    assert (interval + scate.Morning()).isoformat() == "2003-05-11T06:00:00 2003-05-11T12:00:00"
    assert (interval - scate.Morning()).isoformat() == "2002-03-21T06:00:00 2002-03-21T12:00:00"
    assert (interval + scate.Afternoon()).isoformat() == "2003-05-11T12:00:00 2003-05-11T18:00:00"
    assert (interval - scate.Afternoon()).isoformat() == "2002-03-21T12:00:00 2002-03-21T18:00:00"
    assert (interval + scate.Noon()).isoformat() == "2003-05-11T12:00:00 2003-05-11T12:01:00"
    assert (interval - scate.Noon()).isoformat() == "2002-03-21T12:00:00 2002-03-21T12:01:00"
    assert (interval + scate.Evening()).isoformat() == "2003-05-11T18:00:00 2003-05-12T00:00:00"
    assert (interval - scate.Evening()).isoformat() == "2002-03-21T18:00:00 2002-03-22T00:00:00"
    assert (interval + scate.Night()).isoformat() == "2003-05-11T00:00:00 2003-05-11T06:00:00"
    assert (interval - scate.Night()).isoformat() == "2002-03-22T00:00:00 2002-03-22T06:00:00"


def test_union():
    interval = scate.Interval.fromisoformat("2003-01-01T00:00 2003-01-30T00:00")
    feb = scate.Repeating(scate.MONTH, scate.YEAR, value=2)
    day20 = scate.Repeating(scate.DAY, scate.MONTH, value=20)
    union = scate.Union([feb, day20])
    assert (interval - union).isoformat() == "2002-12-20T00:00:00 2002-12-21T00:00:00"
    assert (interval - union - union).isoformat() == "2002-11-20T00:00:00 2002-11-21T00:00:00"
    assert (interval + union).isoformat() == "2003-02-01T00:00:00 2003-03-01T00:00:00"
    assert (interval + union + union).isoformat() == "2003-03-20T00:00:00 2003-03-21T00:00:00"

    interval = scate.Interval.fromisoformat("2011-07-02T00:00 2011-07-31T00:00")
    day = scate.Repeating(scate.DAY)
    month = scate.Repeating(scate.MONTH)
    union = scate.Union([day, month])
    assert (interval - union).isoformat() == "2011-07-01T00:00:00 2011-07-02T00:00:00"
    assert (interval - union - union).isoformat() == "2011-06-01T00:00:00 2011-07-01T00:00:00"
    assert (interval + union).isoformat() == "2011-07-31T00:00:00 2011-08-01T00:00:00"
    assert (interval + union + union).isoformat() == "2011-08-01T00:00:00 2011-09-01T00:00:00"

    # NOTE: In 2001, June 20 and July 25 are Mondays
    interval = scate.Interval.fromisoformat("2011-07-01T00:00 2011-07-19T00:00")
    week = scate.Repeating(scate.WEEK)
    union = scate.Union([week, day20])
    assert (interval - union).isoformat() == "2011-06-20T00:00:00 2011-06-27T00:00:00"
    assert (interval - union - union).isoformat() == "2011-06-13T00:00:00 2011-06-20T00:00:00"
    assert (interval + union).isoformat() == "2011-07-20T00:00:00 2011-07-21T00:00:00"
    assert (interval + union + union).isoformat() == "2011-07-25T00:00:00 2011-08-01T00:00:00"


def test_intersection():
    # Friday the 13ths 2012 - 2023:
    # Friday, January 13, 2012
    # Friday, April 13, 2012
    # Friday, July 13, 2012
    # Friday, September 13, 2013
    # Friday, December 13, 2013
    # Friday, June 13, 2014
    # Friday, February 13, 2015
    # Friday, March 13, 2015
    # Friday, November 13, 2015
    # Friday, May 13, 2016
    # Friday, January 13, 2017
    # Friday, October 13, 2017
    # Friday, April 13, 2018
    # Friday, July 13, 2018
    # Friday, September 13, 2019
    # Friday, December 13, 2019
    # Friday, March 13, 2020
    # Friday, November 13, 2020
    # Friday, August 13, 2021
    # Friday, May 13, 2022
    # Friday, January 13, 2023
    # Friday, October 13, 2023

    y2016 = scate.Year(2016)
    jan_fri_13 = scate.Intersection([
        scate.Repeating(scate.DAY, scate.WEEK, value=scate.FRIDAY),
        scate.Repeating(scate.DAY, scate.MONTH, value=13),
        scate.Repeating(scate.MONTH, scate.YEAR, value=1),
    ])

    assert (y2016 - jan_fri_13).isoformat() == "2012-01-13T00:00:00 2012-01-14T00:00:00"
    assert (y2016 - jan_fri_13 - jan_fri_13).isoformat() == "2006-01-13T00:00:00 2006-01-14T00:00:00"
    assert (y2016 + jan_fri_13).isoformat() == "2017-01-13T00:00:00 2017-01-14T00:00:00"
    assert (y2016 + jan_fri_13 + jan_fri_13).isoformat() == "2023-01-13T00:00:00 2023-01-14T00:00:00"

    fri_13_hours = scate.Intersection([
        scate.Repeating(scate.DAY, scate.WEEK, value=scate.FRIDAY),
        scate.Repeating(scate.DAY, scate.MONTH, value=13),
        scate.Repeating(scate.HOUR),
    ])

    assert (y2016 - fri_13_hours).isoformat() == "2015-11-13T23:00:00 2015-11-14T00:00:00"
    assert (y2016 - fri_13_hours - fri_13_hours).isoformat() == "2015-11-13T22:00:00 2015-11-13T23:00:00"
    interval = y2016
    for _ in range(25):
        interval -= fri_13_hours
    assert interval.isoformat() == "2015-03-13T23:00:00 2015-03-14T00:00:00"
    assert (y2016 + fri_13_hours).isoformat() == "2017-01-13T00:00:00 2017-01-13T01:00:00"
    assert (y2016 + fri_13_hours + fri_13_hours).isoformat() == "2017-01-13T01:00:00 2017-01-13T02:00:00"
    interval = y2016
    for _ in range(25):
        interval += fri_13_hours
    assert interval.isoformat() == "2017-10-13T00:00:00 2017-10-13T01:00:00"

    mar31 = scate.Intersection([
        scate.Repeating(scate.MONTH, scate.YEAR, value=3),
        scate.Repeating(scate.DAY, scate.MONTH, value=31),
    ])
    date = datetime.datetime.fromisoformat("1980-01-01T00:00:00")
    assert (date + mar31).isoformat() == "1980-03-31T00:00:00 1980-04-01T00:00:00"

    apr31 = scate.Intersection([
        scate.Repeating(scate.MONTH, scate.YEAR, value=4),
        scate.Repeating(scate.DAY, scate.MONTH, value=31),
    ])
    with pytest.raises(ValueError):
        date + apr31

    i20120301 = scate.Interval.of(2012, 3, 1)
    eve31 = scate.Intersection([
        scate.Repeating(scate.DAY, scate.MONTH, value=31),
        scate.Evening(),
    ])

    assert (i20120301 - eve31).isoformat() == "2012-01-31T18:00:00 2012-02-01T00:00:00"
    assert (i20120301 + eve31).isoformat() == "2012-03-31T18:00:00 2012-04-01T00:00:00"
    assert (i20120301 + eve31 + eve31).isoformat() == "2012-05-31T18:00:00 2012-06-01T00:00:00"

    m11d25noon = scate.Intersection([
        scate.Repeating(scate.MONTH, scate.YEAR, value=11),
        scate.Repeating(scate.DAY, scate.MONTH, value=25),
        scate.Noon(),
    ])
    assert (scate.Interval.of(2000, 11, 25, 12, 1) + m11d25noon).isoformat() == \
           "2001-11-25T12:00:00 2001-11-25T12:01:00"


def test_year():
    assert scate.Year(1985).isoformat() == "1985-01-01T00:00:00 1986-01-01T00:00:00"
    assert scate.Year(198, 1).isoformat() == "1980-01-01T00:00:00 1990-01-01T00:00:00"
    assert scate.Year(17, 2).isoformat() == "1700-01-01T00:00:00 1800-01-01T00:00:00"


def test_year_suffix():
    assert scate.YearSuffix(scate.Year(1903), 37, 2).isoformat() == scate.Interval.of(1937).isoformat()
    assert scate.YearSuffix(scate.Year(2016), 418, 3).isoformat() == scate.Interval.of(2418).isoformat()
    assert scate.YearSuffix(scate.Year(132, 1), 85, 2).isoformat() == scate.Interval.of(1385).isoformat()
    assert scate.YearSuffix(scate.Year(23, 2), 22, 2).isoformat() == scate.Interval.of(2322).isoformat()
    assert scate.YearSuffix(scate.Year(1903), 3, 1, 1).isoformat() == "1930-01-01T00:00:00 1940-01-01T00:00:00"
    assert scate.YearSuffix(scate.Year(132, 1), 8, 1, 1).isoformat() == "1380-01-01T00:00:00 1390-01-01T00:00:00"
    assert scate.YearSuffix(scate.Year(132, 1), 240, 3, 1).isoformat() == "2400-01-01T00:00:00 2410-01-01T00:00:00"


def test_last():
    period1 = scate.Period(scate.YEAR, 1)
    period2 = scate.Period(scate.YEAR, 2)
    period3 = scate.Period(scate.MONTH, 3)
    period_sum = scate.Sum([period1, period2, period3])

    year = scate.Year(2000)
    assert scate.Last(year, period1).isoformat() == "1999-01-01T00:00:00 2000-01-01T00:00:00"
    assert scate.Last(year, period_sum).isoformat() == "1996-10-01T00:00:00 2000-01-01T00:00:00"

    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    may = scate.Repeating(scate.MONTH, scate.YEAR, value=5)
    friday = scate.Repeating(scate.DAY, scate.WEEK, value=scate.FRIDAY)
    day = scate.Repeating(scate.DAY)
    assert scate.Last(interval, may).isoformat() == "2001-05-01T00:00:00 2001-06-01T00:00:00"
    assert scate.Last(interval, day).isoformat() == "2002-03-21T00:00:00 2002-03-22T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 7), day).isoformat() == \
           "2017-07-06T00:00:00 2017-07-07T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 7), day, interval_included=True).isoformat() == \
           "2017-07-07T00:00:00 2017-07-08T00:00:00"
    # July 7, 2017 is a Friday
    assert scate.Last(scate.Interval.of(2017, 7, 7), friday).isoformat() == \
           "2017-06-30T00:00:00 2017-07-01T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 7), friday, interval_included=True).isoformat() == \
           "2017-07-07T00:00:00 2017-07-08T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 8), friday).isoformat() == \
           "2017-07-07T00:00:00 2017-07-08T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 8), friday, interval_included=True).isoformat() == \
           "2017-07-07T00:00:00 2017-07-08T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 6), friday).isoformat() == \
           "2017-06-30T00:00:00 2017-07-01T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 6), friday, interval_included=True).isoformat() == \
           "2017-06-30T00:00:00 2017-07-01T00:00:00"

    # January 2nd is the first Monday of 2017
    last_week = scate.Last(scate.Interval.of(2017, 1, 9), scate.Repeating(scate.WEEK))
    assert last_week.isoformat() == "2017-01-02T00:00:00 2017-01-09T00:00:00"

    assert scate.Last(interval, scate.Repeating(scate.QUARTER_YEAR)).isoformat() == \
        "2001-10-01T00:00:00 2002-01-01T00:00:00"


def test_n():
    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    may = scate.Repeating(scate.MONTH, scate.YEAR, value=5)
    day = scate.Repeating(scate.DAY)
    month = scate.Repeating(scate.MONTH)

    assert [x.isoformat() for x in scate.LastN(interval, may, 3)] == \
           [f"{y}-05-01T00:00:00 {y}-06-01T00:00:00" for y in [2001, 2000, 1999]]
    assert [x.isoformat() for x in scate.NextN(interval, may, 3)] == \
           [f"{y}-05-01T00:00:00 {y}-06-01T00:00:00" for y in [2004, 2005, 2006]]

    assert [x.isoformat() for x in scate.LastN(interval, day, 5)] == \
           [f"2002-03-{d}T00:00:00 2002-03-{d + 1}T00:00:00" for d in [21, 20, 19, 18, 17]]
    assert [x.isoformat() for x in scate.NextN(interval, day, 5)] == \
           [f"2003-05-{d}T00:00:00 2003-05-{d + 1}T00:00:00" for d in [11, 12, 13, 14, 15]]

    last_day_included = scate.LastN(interval, day, 1, interval_included=True)
    assert [x.isoformat() for x in last_day_included] == ["2003-05-09T00:00:00 2003-05-10T00:00:00"]
    next_3_days_included = scate.NextN(interval, day, 3, interval_included=True)
    assert [x.isoformat() for x in next_3_days_included] == \
           [f"2002-03-{d}T00:00:00 2002-03-{d+1}T00:00:00" for d in [23, 24, 25]]

    # the first 9 months in 1997
    assert [x.isoformat() for x in scate.NthN(scate.Year(1997), month, index=1, n=9)] == \
           [scate.Interval.of(1997, i).isoformat() for i in range(1, 10)]

    # the third two days in 1997
    assert [x.isoformat() for x in scate.NthN(scate.Year(1997), day, index=3, n=2)] == \
           [scate.Interval.of(1997, 1, i).isoformat() for i in range(5, 7)]

    # a few days (n=None)
    assert list(scate.LastN(interval, day, None).isoformats()) == \
           ["2002-03-21T00:00:00 2002-03-22T00:00:00", "... 2002-03-21T00:00:00"]
    assert list(scate.NextN(interval, day, None).isoformats()) == \
           ["2003-05-11T00:00:00 2003-05-12T00:00:00", "2003-05-12T00:00:00 ..."]
    assert list(scate.NthN(interval, day, index=1, n=None).isoformats()) == \
           ["2002-03-23T00:00:00 2002-03-24T00:00:00", "2002-03-24T00:00:00 ..."]
    assert list(scate.NthN(interval, day, index=1, n=None, from_end=True).isoformats()) == \
           ["2003-05-09T00:00:00 2003-05-10T00:00:00", "... 2003-05-09T00:00:00"]


def test_next():
    year1 = scate.Year(2000)
    period1 = scate.Period(scate.YEAR, 1)
    assert scate.Next(year1, period1).isoformat() == "2001-01-01T00:00:00 2002-01-01T00:00:00"
    date2 = scate.Interval.of(2017, 8, 16)
    period2 = scate.Period(scate.WEEK, 2)
    assert scate.Next(date2, period2).isoformat() == "2017-08-17T00:00:00 2017-08-31T00:00:00"
    year3 = scate.Year(2000)
    period3 = scate.Sum([scate.Period(scate.YEAR, 1),
                         scate.Period(scate.YEAR, 2),
                         scate.Period(scate.MONTH, 3)])
    assert scate.Next(year3, period3).isoformat() == "2001-01-01T00:00:00 2004-04-01T00:00:00"

    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    may = scate.Repeating(scate.MONTH, scate.YEAR, value=5)
    day = scate.Repeating(scate.DAY)
    assert scate.Next(interval, may).isoformat() == "2004-05-01T00:00:00 2004-06-01T00:00:00"
    assert scate.Next(interval, day).isoformat() == "2003-05-11T00:00:00 2003-05-12T00:00:00"
    # January 2nd is the first Monday of 2017
    next_week = scate.Next(scate.Interval.of(2017, 1, 8), scate.Repeating(scate.WEEK))
    assert next_week.isoformat() == "2017-01-09T00:00:00 2017-01-16T00:00:00"
    assert scate.Next(interval, may, interval_included=True).isoformat() == \
           "2002-05-01T00:00:00 2002-06-01T00:00:00"


def test_before():
    period1 = scate.Period(scate.YEAR, 1)
    period2 = scate.Sum([period1,
                         scate.Period(scate.YEAR, 2),
                         scate.Period(scate.MONTH, 3)])
    period3 = scate.Period(scate.WEEK, 2)
    year = scate.Year(2000)
    assert scate.Before(year, period1).isoformat() == "1999-01-01T00:00:00 2000-01-01T00:00:00"
    assert scate.Before(year, period2).isoformat() == "1996-10-01T00:00:00 1997-10-01T00:00:00"

    date = scate.Interval.of(2017, 7, 28)
    assert scate.Before(date, period3).isoformat() == "2017-07-14T00:00:00 2017-07-15T00:00:00"
    # when expanding, 2 weeks Before July 28 is the 7-day interval around July 14
    assert period3.unit.expand(scate.Before(date, period3)).isoformat() == \
           "2017-07-11T00:00:00 2017-07-18T00:00:00"

    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    may = scate.Repeating(scate.MONTH, scate.YEAR, value=5)
    day = scate.Repeating(scate.DAY)
    assert scate.Before(interval, may).isoformat() == "2001-05-01T00:00:00 2001-06-01T00:00:00"
    assert scate.Before(interval, may, interval_included=True).isoformat() == \
           "2002-05-01T00:00:00 2002-06-01T00:00:00"
    assert scate.Before(interval, may, 5).isoformat() == "1997-05-01T00:00:00 1997-06-01T00:00:00"
    assert scate.Before(interval, day).isoformat() == "2002-03-21T00:00:00 2002-03-22T00:00:00"
    assert scate.Before(interval, day, interval_included=True).isoformat() == \
           "2003-05-09T00:00:00 2003-05-10T00:00:00"
    assert scate.Before(interval, day, 20).isoformat() == "2002-03-02T00:00:00 2002-03-03T00:00:00"

    assert scate.Before(interval, None).isoformat() == "... 2002-03-22T11:30:30"


def test_after():
    year = scate.Period(scate.YEAR, 1)
    month = scate.Period(scate.MONTH, 3)
    year3month3 = scate.Sum([year, scate.Period(scate.YEAR, 2), scate.Period(scate.MONTH, 3)])

    interval = scate.Year(2000)
    assert scate.After(interval, year).isoformat() == "2001-01-01T00:00:00 2002-01-01T00:00:00"
    assert scate.After(interval, year, 2).isoformat() == "2002-01-01T00:00:00 2003-01-01T00:00:00"

    assert scate.After(interval, year3month3).isoformat() == "2003-04-01T00:00:00 2004-04-01T00:00:00"
    assert scate.After(interval, year3month3, 3).isoformat() == "2009-10-01T00:00:00 2010-10-01T00:00:00"

    interval = scate.Interval.of(2000, 1, 25)
    assert scate.After(interval, month).isoformat() == "2000-04-25T00:00:00 2000-04-26T00:00:00"
    assert scate.After(interval, month).isoformat() == "2000-04-25T00:00:00 2000-04-26T00:00:00"
    # when expanding, 3 months After January 25 is the 1-month interval around April 25
    assert month.unit.expand(scate.After(interval, month)).isoformat() == \
           "2000-04-10T12:00:00 2000-05-10T12:00:00"

    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    may = scate.Repeating(scate.MONTH, scate.YEAR, value=5)
    day = scate.Repeating(scate.DAY)

    assert scate.After(interval, may).isoformat() == "2004-05-01T00:00:00 2004-06-01T00:00:00"
    assert scate.After(interval, may, interval_included=True).isoformat() == \
           "2002-05-01T00:00:00 2002-06-01T00:00:00"
    assert scate.After(interval, may).isoformat() == "2004-05-01T00:00:00 2004-06-01T00:00:00"
    assert scate.After(interval, day).isoformat() == "2003-05-11T00:00:00 2003-05-12T00:00:00"
    assert scate.After(interval, day, interval_included=True).isoformat() == \
           "2002-03-23T00:00:00 2002-03-24T00:00:00"
    assert scate.After(interval, day, 11).isoformat() == "2003-05-21T00:00:00 2003-05-22T00:00:00"

    assert scate.After(interval, None).isoformat() == "2003-05-10T22:10:20 ..."


def test_this():
    period1 = scate.Period(scate.YEAR, 1)
    interval = scate.Year(2002)
    assert scate.This(interval, period1).isoformat() == "2002-01-01T00:00:00 2003-01-01T00:00:00"

    interval = scate.Interval.fromisoformat("2001-01-01T00:00:00 2001-01-01T00:00:00")
    period2 = scate.Period(scate.DAY, 5)
    assert scate.This(interval, period2).isoformat() == "2000-12-29T12:00:00 2001-01-03T12:00:00"

    interval = scate.Year(2016)
    april = scate.Repeating(scate.MONTH, scate.YEAR, value=4)
    day = scate.Repeating(scate.DAY)
    assert scate.This(interval, april).isoformat() == "2016-04-01T00:00:00 2016-05-01T00:00:00"
    with pytest.raises(ValueError):
        scate.This(interval, day)

    interval = scate.Interval.fromisoformat("2016-07-01T00:00:00 2016-07-02T00:00:00")
    month = scate.Repeating(scate.MONTH)
    assert scate.This(interval, month).isoformat() == "2016-07-01T00:00:00 2016-08-01T00:00:00"
    assert scate.This(interval, scate.Summer()).isoformat() == "2016-06-01T00:00:00 2016-09-01T00:00:00"
    assert scate.This(interval, scate.Winter()).isoformat() == "2016-12-01T00:00:00 2017-03-01T00:00:00"
    assert scate.This(interval, scate.Night()).isoformat() == "2016-07-01T00:00:00 2016-07-01T06:00:00"

    interval = scate.Interval.fromisoformat("2016-07-01T10:00:00 2016-07-01T11:00:00")
    assert scate.This(interval, scate.Noon()).isoformat() == "2016-07-01T12:00:00 2016-07-01T12:01:00"


def test_between():
    year1 = scate.Year(1999)
    year2 = scate.Year(2002)
    assert scate.Between(year1, year2).isoformat() == "2000-01-01T00:00:00 2002-01-01T00:00:00"
    assert scate.Between(year1, year2, start_included=True).isoformat() == \
           "1999-01-01T00:00:00 2002-01-01T00:00:00"
    assert scate.Between(year1, year2, end_included=True).isoformat() == \
           "2000-01-01T00:00:00 2003-01-01T00:00:00"
    assert scate.Between(year1, year2, start_included=True, end_included=True).isoformat() == \
           "1999-01-01T00:00:00 2003-01-01T00:00:00"

    # it's an error for the start interval to be after the end interval
    with pytest.raises(ValueError):
        scate.Between(year2, year1)


def test_nth():
    y2001 = scate.Year(2001)
    month = scate.Period(scate.MONTH, 1)
    year = scate.Period(scate.YEAR, 1)
    period = scate.Sum([month, scate.Period(scate.MINUTE, 20)])
    assert scate.Nth(y2001, year, 1).isoformat() == "2001-01-01T00:00:00 2002-01-01T00:00:00"
    assert scate.Nth(y2001, month, 2).isoformat() == "2001-02-01T00:00:00 2001-03-01T00:00:00"
    assert scate.Nth(y2001, month, 2, from_end=True).isoformat() == "2001-11-01T00:00:00 2001-12-01T00:00:00"
    assert scate.Nth(y2001, period, 4).isoformat() == "2001-04-01T01:00:00 2001-05-01T01:20:00"
    with pytest.raises(ValueError):
        scate.Nth(y2001, year, 2)

    interval = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-05-10T22:10:20")
    quarter_year = scate.Repeating(scate.QUARTER_YEAR)
    may = scate.Repeating(scate.MONTH, scate.YEAR, value=5)
    day = scate.Repeating(scate.DAY)
    assert scate.Nth(y2001, quarter_year, 4).isoformat() == "2001-10-01T00:00:00 2002-01-01T00:00:00"
    assert scate.Nth(interval, may, 1).isoformat() == "2002-05-01T00:00:00 2002-06-01T00:00:00"
    assert scate.Nth(interval, may, 1, from_end=True).isoformat() == "2002-05-01T00:00:00 2002-06-01T00:00:00"
    assert scate.Nth(interval, day, 3).isoformat() == "2002-03-25T00:00:00 2002-03-26T00:00:00"
    assert scate.Nth(interval, day, 3, from_end=True).isoformat() == "2003-05-07T00:00:00 2003-05-08T00:00:00"
    with pytest.raises(ValueError):
        scate.Nth(interval, may, 5)
    with pytest.raises(ValueError):
        scate.Nth(interval, may, 2, from_end=True)


def test_these():

    # These(Tue 1 Feb, Fri) => Fri 4 Feb
    interval_tue = scate.Interval.fromisoformat("2005-02-01T03:22 2005-02-02T00:00")
    friday = scate.Repeating(scate.DAY, scate.WEEK, value=scate.FRIDAY)
    assert [x.isoformat() for x in scate.These(interval_tue, friday)] == \
           [scate.Interval.of(2005, 2, 4).isoformat()]

    # These(Sat 8 Mar until Fri 14 Mar, Fri) => Fri 7 Mar, Fri 14 Mar
    interval_week_sat = scate.Interval.fromisoformat("2003-03-08 2003-03-14")
    assert [x.isoformat() for x in scate.These(interval_week_sat, friday)] == \
           [scate.Interval.of(2003, 3, x).isoformat() for x in [7, 14]]

    # These(Thu 10 Apr until Thu 17 Apr, Fri) => Fri 11 Apr, Fri 18 Apr
    interval_week_thu = scate.Interval.fromisoformat("2003-04-10 2003-04-17")
    assert [x.isoformat() for x in scate.These(interval_week_thu, friday)] == \
           [scate.Interval.of(2003, 4, x).isoformat() for x in [11, 18]]

    # These(22 Mar 2002 until 10 Feb 2003, Mar) => Mar 2002, Mar 2003
    interval_11_months = scate.Interval.fromisoformat("2002-03-22T11:30:30 2003-02-10T22:10:20")
    march = scate.Repeating(scate.MONTH, scate.YEAR, value=3)
    assert [x.isoformat() for x in scate.These(interval_11_months, march)] == \
           [scate.Interval.of(x, 3).isoformat() for x in [2002, 2003]]

    # These(Thu 10 Apr until Thu 17 Apr, Mar) => Mar 2003
    assert [x.isoformat() for x in scate.These(interval_week_thu, march)] == \
           [scate.Interval.of(2003, 3).isoformat()]

    # These(22 Mar 2002 until 10 Feb 2003, Fri) => ... 48 Fridays ...
    assert len(list(scate.These(interval_11_months, friday))) == 48

    # These(Tue 1 Feb, Week) => Mon 31 Jan through Sun 6 Feb
    week = scate.Repeating(scate.WEEK)
    assert [x.isoformat() for x in scate.These(interval_tue, week)] == \
           ["2005-01-31T00:00:00 2005-02-07T00:00:00"]

    # These(Thu 10 Apr until Thu 17 Apr, Mar) => Mar 2003
    month = scate.Repeating(scate.MONTH)
    assert [x.isoformat() for x in scate.These(interval_week_thu, month)] == \
           [scate.Interval.of(2003, 4).isoformat()]

    # These(22 Mar 2002 until 10 Feb 2003, Year) => 2002, 2003
    year = scate.Repeating(scate.YEAR)
    assert [x.isoformat() for x in scate.These(interval_11_months, year)] == \
           [scate.Year(x).isoformat() for x in [2002, 2003]]

    # These(Thu 10 Apr until Thu 17 Apr, day) => ... 7 days ...
    day = scate.Repeating(scate.DAY)
    assert len(list(scate.These(interval_week_thu, day))) == 7


def test_misc():
    # PRI19980216.2000.0170 (349,358) last week
    week = scate.Repeating(scate.WEEK)
    assert scate.Last(scate.Interval.of(1998, 2, 16), week).isoformat() == \
           "1998-02-09T00:00:00 1998-02-16T00:00:00"

    # APW19980322.0749 (988,994) Monday
    monday = scate.Repeating(scate.DAY, scate.WEEK, value=scate.MONDAY)
    assert scate.Next(scate.Interval.of(1998, 3, 22, 14, 57), monday).isoformat() == \
           "1998-03-23T00:00:00 1998-03-24T00:00:00"

    # APW19990206.0090 (767,781) Thursday night
    # NOTE: as written, this is the night early on Thursday (1999-02-04)
    # to get the night early on Friday (1999-02-05), a Next would be needed
    thursday = scate.Repeating(scate.DAY, scate.WEEK, value=scate.THURSDAY)
    thursday_night = scate.Intersection([thursday, scate.Night()])
    assert scate.Last(scate.Interval.of(1999, 2, 6, 6, 22, 26), thursday_night).isoformat() == \
           "1999-02-04T00:00:00 1999-02-04T06:00:00"

    # wsj_0124 (450,457) Nov. 13
    nov13 = scate.Intersection([
        scate.Repeating(scate.MONTH, scate.YEAR, value=11),
        scate.Repeating(scate.DAY, scate.MONTH, value=13),
    ])
    assert scate.Next(scate.Interval.of(1989, 11, 2), nov13).isoformat() == \
        scate.Interval.of(1989, 11, 13).isoformat()
    assert scate.Last(scate.Interval.of(1989, 11, 14), nov13).isoformat() == \
        scate.Interval.of(1989, 11, 13).isoformat()
    assert scate.Next(scate.Interval.of(1989, 11, 12), nov13).isoformat() == \
        scate.Interval.of(1989, 11, 13).isoformat()

    # NYT19980206.0460 (2979,3004) first nine months of 1997
    month = scate.Repeating(scate.MONTH)
    assert [x.isoformat() for x in scate.NthN(scate.Year(1997), month, 1, 9)] == \
        [scate.Interval.of(1997, m).isoformat() for m in range(1, 10)]

    # wsj_0346 (889,894) year ended March 31
    march = scate.Repeating(scate.MONTH, scate.YEAR, value=3)
    day31 = scate.Repeating(scate.DAY, scate.MONTH, value=31)
    march31 = scate.Intersection([march, day31])
    year = scate.Period(scate.YEAR, 1)
    assert scate.Last(scate.Last(scate.Interval.of(1989, 11, 1), march31), year).isoformat() == \
            "1988-03-31T00:00:00 1989-03-31T00:00:00"
