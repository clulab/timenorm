import scate
import datetime
from time_unit import TimeUnit


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
    period = scate.Period("year", 5)
    assert period.add_to(date).isoformat() == "2005-01-01T00:00:00" 
    assert period.subtract_from(date).isoformat() == "1995-01-01T00:00:00" 

def test_truncate():
    date = datetime.datetime(2026, 2, 3, 1, 7, 35, 30)
    assert TimeUnit.truncate(date, TimeUnit.QUARTER_CENTURY).isoformat() == "2025-01-01T00:00:00"
    assert TimeUnit.truncate(date, TimeUnit.CENTURY).isoformat() == "2000-01-01T00:00:00"
    assert TimeUnit.truncate(date, TimeUnit.DECADE).isoformat() == "2020-01-01T00:00:00"
    date2 = datetime.datetime(2022, 10, 27, 7, 0, 0, 0)
    assert TimeUnit.truncate(date2, TimeUnit.WEEK).isoformat() == "2022-10-24T00:00:00"
    # TODO: ask Steve about this, isoformat doesn't include milliseconds? Or should it?
    # assert TimeUnit.truncate(date2, TimeUnit.MILLISECOND).isoformat() == "2022-10-27T07:00:00.000"

def test_this_p():
    period1 = scate.Period("year", 1)
    year = scate.Year(2002)
    this_period = scate.ThisP(year, period1)
    assert this_period.start.isoformat() == "2002-01-01T00:00:00"
    assert this_period.end.isoformat() == "2003-01-01T00:00:00"
    interval = scate.Interval(datetime.datetime(2001, 1, 1), datetime.datetime(2001, 1, 1))
    period2 = scate.Period("day", 5)
    this_period2 = scate.ThisP(interval, period2)
    assert this_period2.start.isoformat() == "2000-12-29T12:00:00"
    assert this_period2.end.isoformat() == "2001-01-03T12:00:00"

def test_repeating_unit():
    repeating_unit = scate.RepeatingUnit(TimeUnit.YEAR)
    preceding = repeating_unit.preceding(datetime.datetime(2000, 1, 1))
    assert next(preceding).start.isoformat() == "1999-01-01T00:00:00"
    assert next(preceding).start.isoformat() == "1998-01-01T00:00:00"

