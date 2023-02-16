import scate
import datetime


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
    period = scate.Period(scate.TimeUnit.YEAR, 5)
    assert period.add_to(date).isoformat() == "2005-01-01T00:00:00" 
    assert period.subtract_from(date).isoformat() == "1995-01-01T00:00:00" 


def test_truncate():
    date = datetime.datetime(2026, 2, 3, 1, 7, 35, 30)
    assert scate.TimeUnit.truncate(date, scate.TimeUnit.QUARTER_CENTURY).isoformat() == "2025-01-01T00:00:00"
    assert scate.TimeUnit.truncate(date, scate.TimeUnit.CENTURY).isoformat() == "2000-01-01T00:00:00"
    assert scate.TimeUnit.truncate(date, scate.TimeUnit.DECADE).isoformat() == "2020-01-01T00:00:00"
    date2 = datetime.datetime(2022, 10, 27, 7, 0, 0, 0)
    assert scate.TimeUnit.truncate(date2, scate.TimeUnit.WEEK).isoformat() == "2022-10-24T00:00:00"
    # TODO: ask Steve about this, isoformat doesn't include milliseconds? Or should it?
    # assert TimeUnit.truncate(date2, TimeUnit.MILLISECOND).isoformat() == "2022-10-27T07:00:00.000"


def test_this_p():
    period1 = scate.Period(scate.TimeUnit.YEAR, 1)
    year = scate.Year(2002)
    this_period = scate.ThisP(year, period1)
    assert this_period.start.isoformat() == "2002-01-01T00:00:00"
    assert this_period.end.isoformat() == "2003-01-01T00:00:00"
    interval = scate.Interval(datetime.datetime(2001, 1, 1), datetime.datetime(2001, 1, 1))
    period2 = scate.Period(scate.TimeUnit.DAY, 5)
    this_period2 = scate.ThisP(interval, period2)
    assert this_period2.start.isoformat() == "2000-12-29T12:00:00"
    assert this_period2.end.isoformat() == "2001-01-03T12:00:00"


def test_repeating_unit():
    repeating_unit = scate.RepeatingUnit(scate.TimeUnit.YEAR)
    preceding = repeating_unit.preceding(datetime.datetime(2000, 1, 1))
    assert next(preceding).start.isoformat() == "1999-01-01T00:00:00"
    assert next(preceding).start.isoformat() == "1998-01-01T00:00:00"


def test_repeating_field():
    interval = scate.Interval(datetime.datetime(2002, 3, 22, 11, 30, 30, 0),
                              datetime.datetime(2003, 5, 10, 22, 10, 20, 0))
    month_may = scate.RepeatingField(scate.TimeUnit.MONTH_OF_YEAR, 5)
    pre = month_may.preceding(interval.start)
    assert next(pre).isoformat() == "2001-05-01T00:00:00 2001-06-01T00:00:00"
    assert next(pre).isoformat() == "2000-05-01T00:00:00 2000-06-01T00:00:00"
    post = month_may.following(interval.end)
    assert next(post).isoformat() == "2004-05-01T00:00:00 2004-06-01T00:00:00"
    assert next(post).isoformat() == "2005-05-01T00:00:00 2005-06-01T00:00:00"

    day29 = scate.RepeatingField(scate.TimeUnit.DAY_OF_MONTH, 29)
    pre2 = day29.preceding(interval.start)
    assert next(pre2).isoformat() == "2002-01-29T00:00:00 2002-01-30T00:00:00"
    assert next(pre2).isoformat() == "2001-12-29T00:00:00 2001-12-30T00:00:00"
    post2 = day29.following(interval.end)
    assert next(post2).isoformat() == "2003-05-29T00:00:00 2003-05-30T00:00:00"
    assert next(post2).isoformat() == "2003-06-29T00:00:00 2003-06-30T00:00:00"

    # make sure that preceding and following are strict (no overlap allowed)
    nov = scate.RepeatingField(scate.TimeUnit.MONTH_OF_YEAR, 11)
    nov_start = datetime.datetime(1989, 11, 2)
    assert next(nov.preceding(nov_start)).isoformat() == "1988-11-01T00:00:00 1988-12-01T00:00:00"
    assert next(nov.following(nov_start)).isoformat() == "1990-11-01T00:00:00 1990-12-01T00:00:00"

    day31 = scate.RepeatingField(scate.TimeUnit.DAY_OF_MONTH, 31)
    start_of_mar_1980 = datetime.datetime(1980, 3, 1)
    assert next(day31.following(start_of_mar_1980)).isoformat() == "1980-03-31T00:00:00 1980-04-01T00:00:00"
    feb15 = datetime.datetime(2000, 2, 15)
    assert next(day31.following(feb15)).isoformat() == "2000-03-31T00:00:00 2000-04-01T00:00:00"
    assert next(day31.preceding(feb15)).isoformat() == "2000-01-31T00:00:00 2000-02-01T00:00:00"
