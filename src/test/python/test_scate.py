import scate
import datetime
import dateutil.rrule
import pytest


def test_interval():
    assert scate.Interval.of(1985).isoformat() == "1985-01-01T00:00:00 1986-01-01T00:00:00"
    assert scate.Interval.of(1985, 6).isoformat() == "1985-06-01T00:00:00 1985-07-01T00:00:00"
    assert scate.Interval.of(1985, 6, 17).isoformat() == "1985-06-17T00:00:00 1985-06-18T00:00:00"
    assert scate.Interval.of(1985, 6, 17, 23).isoformat() == "1985-06-17T23:00:00 1985-06-18T00:00:00"
    assert scate.Interval.of(1985, 6, 17, 23, 0).isoformat() == "1985-06-17T23:00:00 1985-06-17T23:01:00"


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


def test_period():
    date = datetime.datetime(2000, 1, 1, 0, 0, 0, 0)
    period = scate.Period(scate.Unit.YEAR, 5)
    assert (date + period).end.isoformat() == "2005-01-01T00:00:00"
    assert (date - period).start.isoformat() == "1995-01-01T00:00:00"


def test_sum():
    period1 = scate.Period(scate.Unit.YEAR, 1)
    period2 = scate.Period(scate.Unit.YEAR, 2)
    period3 = scate.Period(scate.Unit.MONTH, 3)
    period4 = scate.Period(scate.Unit.DAY, 2)
    period_sum = scate.Sum([period1, period2, period3])
    dt = datetime.datetime(2000, 6, 10, 0, 0, 0, 0)

    assert (dt + period_sum).end == datetime.datetime(2003, 9, 10, 0, 0, 0, 0)
    assert (dt - period_sum).start == datetime.datetime(1997, 3, 10, 0, 0, 0, 0)

    period_sum2 = scate.Sum([period4, period_sum])

    assert (dt + period_sum2).end == datetime.datetime(2003, 9, 12, 0, 0, 0, 0)
    assert (dt - period_sum2).start == datetime.datetime(1997, 3, 8, 0, 0, 0, 0)


def test_last():
    period1 = scate.Period(scate.Unit.YEAR, 1)
    period2 = scate.Period(scate.Unit.YEAR, 2)
    period3 = scate.Period(scate.Unit.MONTH, 3)
    period_sum = scate.Sum([period1, period2, period3])

    year = scate.Year(2000)
    assert scate.Last(year, period1).isoformat() == "1999-01-01T00:00:00 2000-01-01T00:00:00"
    assert scate.Last(year, period_sum).isoformat() == "1996-10-01T00:00:00 2000-01-01T00:00:00"

    interval = scate.Interval(datetime.datetime(2002, 3, 22, 11, 30, 30, 0),
                              datetime.datetime(2003, 5, 10, 22, 10, 20, 0))
    may = scate.RepeatingField(scate.Field.MONTH_OF_YEAR, 5)
    friday = scate.RepeatingField(scate.Field.DAY_OF_WEEK, dateutil.rrule.FR)
    day = scate.RepeatingUnit(scate.Unit.DAY)
    assert scate.Last(interval, may).isoformat() == "2001-05-01T00:00:00 2001-06-01T00:00:00"
    assert scate.Last(interval, day).isoformat() == "2002-03-21T00:00:00 2002-03-22T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 7), day).isoformat() == \
           "2017-07-06T00:00:00 2017-07-07T00:00:00"
    # July 7, 2017 is a Friday
    assert scate.Last(scate.Interval.of(2017, 7, 7), friday).isoformat() == \
           "2017-06-30T00:00:00 2017-07-01T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 8), friday).isoformat() == \
           "2017-07-07T00:00:00 2017-07-08T00:00:00"
    assert scate.Last(scate.Interval.of(2017, 7, 6), friday).isoformat() == \
           "2017-06-30T00:00:00 2017-07-01T00:00:00"


def test_next():
    year1 = scate.Year(2000)
    period1 = scate.Period(scate.Unit.YEAR, 1)
    assert scate.Next(year1, period1).isoformat() == "2001-01-01T00:00:00 2002-01-01T00:00:00"
    date2 = scate.Interval.of(2017, 8, 16)
    period2 = scate.Period(scate.Unit.WEEK, 2)
    assert scate.Next(date2, period2).isoformat() == "2017-08-17T00:00:00 2017-08-31T00:00:00"
    year3 = scate.Year(2000)
    period3 = scate.Sum([scate.Period(scate.Unit.YEAR, 1),
                         scate.Period(scate.Unit.YEAR, 2),
                         scate.Period(scate.Unit.MONTH, 3)])
    assert scate.Next(year3, period3).isoformat() == "2001-01-01T00:00:00 2004-04-01T00:00:00"


def test_before():
    period1 = scate.Period(scate.Unit.YEAR, 1)
    period2 = scate.Sum([period1,
                         scate.Period(scate.Unit.YEAR, 2),
                         scate.Period(scate.Unit.MONTH, 3)])
    period3 = scate.Period(scate.Unit.WEEK, 2)
    year = scate.Year(2000)
    assert scate.Before(year, period1).isoformat() == "1999-01-01T00:00:00 2000-01-01T00:00:00"
    assert scate.Before(year, period2).isoformat() == "1996-10-01T00:00:00 1997-10-01T00:00:00"

    date = scate.Interval.of(2017, 7, 28)
    assert scate.Before(date, period3).isoformat() == "2017-07-14T00:00:00 2017-07-15T00:00:00"
    # when expanding, 2 weeks Before July 28 is the 7-day interval around July 14
    assert scate.Before(date, period3, expand=True).isoformat() == \
           "2017-07-11T00:00:00 2017-07-18T00:00:00"


def test_after():
    period1 = scate.Period(scate.Unit.YEAR, 1)
    period2 = scate.Sum([period1,
                         scate.Period(scate.Unit.YEAR, 2),
                         scate.Period(scate.Unit.MONTH, 3)])
    period3 = scate.Period(scate.Unit.MONTH, 3)

    year = scate.Year(2000)
    assert scate.After(year, period1).isoformat() == "2001-01-01T00:00:00 2002-01-01T00:00:00"
    assert scate.After(year, period2).isoformat() == "2003-04-01T00:00:00 2004-04-01T00:00:00"

    date = scate.Interval.of(2000, 1, 25)
    assert scate.After(date, period3).isoformat() == "2000-04-25T00:00:00 2000-04-26T00:00:00"
    # when expanding, 3 months After January 25 is the 1-month interval around April 25
    assert scate.After(date, period3, expand=True).isoformat() == \
           "2000-04-10T12:00:00 2000-05-10T12:00:00"


def test_this():
    period1 = scate.Period(scate.Unit.YEAR, 1)
    year = scate.Year(2002)
    assert scate.This(year, period1).isoformat() == "2002-01-01T00:00:00 2003-01-01T00:00:00"
    interval = scate.Interval(datetime.datetime(2001, 1, 1), datetime.datetime(2001, 1, 1))
    period2 = scate.Period(scate.Unit.DAY, 5)
    assert scate.This(interval, period2).isoformat() == "2000-12-29T12:00:00 2001-01-03T12:00:00"


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
    year = scate.Period(scate.Unit.YEAR, 1)
    period = scate.Sum([scate.Period(scate.Unit.YEAR, 1), scate.Period(scate.Unit.MINUTE, 20)])
    assert scate.NthFromStart(y2001, 2, year).isoformat() == "2002-01-01T00:00:00 2003-01-01T00:00:00"
    assert scate.NthFromEnd(y2001, 2, year).isoformat() == "2000-01-01T00:00:00 2001-01-01T00:00:00"
    assert scate.NthFromStart(y2001, 4, period).isoformat() == "2004-01-01T01:00:00 2005-01-01T01:20:00"


def test_truncate():
    date = datetime.datetime(2026, 5, 3, 1, 7, 35, 1111)
    assert scate.Unit.CENTURY.truncate(date).isoformat() == "2000-01-01T00:00:00"
    assert scate.Unit.QUARTER_CENTURY.truncate(date).isoformat() == "2025-01-01T00:00:00"
    assert scate.Unit.DECADE.truncate(date).isoformat() == "2020-01-01T00:00:00"
    assert scate.Unit.YEAR.truncate(date).isoformat() == "2026-01-01T00:00:00"
    assert scate.Unit.QUARTER_YEAR.truncate(date).isoformat() == "2026-04-01T00:00:00"
    assert scate.Unit.MONTH.truncate(date).isoformat() == "2026-05-01T00:00:00"
    assert scate.Unit.WEEK.truncate(date).isoformat() == "2026-04-27T00:00:00"
    assert scate.Unit.DAY.truncate(date).isoformat() == "2026-05-03T00:00:00"
    assert scate.Unit.HOUR.truncate(date).isoformat() == "2026-05-03T01:00:00"
    assert scate.Unit.MINUTE.truncate(date).isoformat() == "2026-05-03T01:07:00"
    assert scate.Unit.SECOND.truncate(date).isoformat() == "2026-05-03T01:07:35"
    assert scate.Unit.MILLISECOND.truncate(date).isoformat() == "2026-05-03T01:07:35.001000"
    assert scate.Unit.MICROSECOND.truncate(date).isoformat() == "2026-05-03T01:07:35.001111"


def test_repeating_unit():
    century = scate.RepeatingUnit(scate.Unit.CENTURY)
    decade = scate.RepeatingUnit(scate.Unit.DECADE)
    year = scate.RepeatingUnit(scate.Unit.YEAR)
    month = scate.RepeatingUnit(scate.Unit.MONTH)
    week = scate.RepeatingUnit(scate.Unit.WEEK)
    day = scate.RepeatingUnit(scate.Unit.DAY)

    interval = scate.Interval.of(2000, 1, 1)
    assert (interval - year).isoformat() == "1999-01-01T00:00:00 2000-01-01T00:00:00"
    assert (interval - year - year).isoformat() == "1998-01-01T00:00:00 1999-01-01T00:00:00"
    assert (interval + day).isoformat() == "2000-01-02T00:00:00 2000-01-03T00:00:00"
    assert (interval + day + day).isoformat() == "2000-01-03T00:00:00 2000-01-04T00:00:00"

    interval = scate.Interval(datetime.datetime(2002, 3, 22, 11, 30, 30, 0),
                              datetime.datetime(2003, 5, 11, 22, 10, 20, 0))
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

    interval = scate.Interval(datetime.datetime(2001, 2, 12, 3, 3),
                              datetime.datetime(2001, 2, 14, 22, 0))
    assert (interval - day).isoformat() == scate.Interval.of(2001, 2, 11).isoformat()
    assert (interval + day).isoformat() == scate.Interval.of(2001, 2, 15).isoformat()

    interval = scate.Interval(datetime.datetime(2001, 2, 12, 0, 0),
                              datetime.datetime(2001, 2, 14, 0, 0))
    assert (interval - day).isoformat() == scate.Interval.of(2001, 2, 11).isoformat()
    assert (interval + day).isoformat() == scate.Interval.of(2001, 2, 14).isoformat()

    # December 31, 2012 is a Monday
    interval = scate.Interval.of(2013, 1, 8)
    assert (interval - week).isoformat() == "2012-12-31T00:00:00 2013-01-07T00:00:00"


def test_repeating_field():
    interval = scate.Interval(datetime.datetime(2002, 3, 22, 11, 30, 30, 0),
                              datetime.datetime(2003, 5, 10, 22, 10, 20, 0))
    may = scate.RepeatingField(scate.Field.MONTH_OF_YEAR, 5)
    assert (interval - may).isoformat() == "2001-05-01T00:00:00 2001-06-01T00:00:00"
    assert (interval - may - may).isoformat() == "2000-05-01T00:00:00 2000-06-01T00:00:00"
    assert (interval + may).isoformat() == "2004-05-01T00:00:00 2004-06-01T00:00:00"
    assert (interval + may + may).isoformat() == "2005-05-01T00:00:00 2005-06-01T00:00:00"

    day29 = scate.RepeatingField(scate.Field.DAY_OF_MONTH, 29)
    assert (interval - day29).isoformat() == "2002-01-29T00:00:00 2002-01-30T00:00:00"
    assert (interval - day29 - day29).isoformat() == "2001-12-29T00:00:00 2001-12-30T00:00:00"
    assert (interval + day29).isoformat() == "2003-05-29T00:00:00 2003-05-30T00:00:00"
    assert (interval + day29 + day29).isoformat() == "2003-06-29T00:00:00 2003-06-30T00:00:00"

    # make sure that preceding and following are strict (no overlap allowed)
    nov = scate.RepeatingField(scate.Field.MONTH_OF_YEAR, 11)
    interval = scate.Interval.of(1989, 11, 2)
    assert (interval - nov).isoformat() == "1988-11-01T00:00:00 1988-12-01T00:00:00"
    assert (interval + nov).isoformat() == "1990-11-01T00:00:00 1990-12-01T00:00:00"

    day31 = scate.RepeatingField(scate.Field.DAY_OF_MONTH, 31)
    interval = scate.Interval.of(1980, 3, 1)
    assert (interval + day31).isoformat() == "1980-03-31T00:00:00 1980-04-01T00:00:00"
    interval = scate.Interval.of(2000, 2, 15)
    assert (interval + day31).isoformat() == "2000-03-31T00:00:00 2000-04-01T00:00:00"
    assert (interval - day31).isoformat() == "2000-01-31T00:00:00 2000-02-01T00:00:00"
