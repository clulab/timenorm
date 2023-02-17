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
    period = scate.Period(scate.Unit.YEAR, 5)
    assert (date + period).isoformat() == "2005-01-01T00:00:00"
    assert (date - period).isoformat() == "1995-01-01T00:00:00"


def test_sum():
    period1 = scate.Period(scate.Unit.YEAR, 1)
    period2 = scate.Period(scate.Unit.YEAR, 2)
    period3 = scate.Period(scate.Unit.MONTH, 3)
    period4 = scate.Period(scate.Unit.DAY, 2)
    period_sum = scate.Sum([period1, period2, period3])
    dt = datetime.datetime(2000, 6, 10, 0, 0, 0, 0)

    assert dt + period_sum == datetime.datetime(2003, 9, 10, 0, 0, 0, 0)
    assert dt - period_sum == datetime.datetime(1997, 3, 10, 0, 0, 0, 0)

    period_sum2 = scate.Sum([period4, period_sum])

    assert dt + period_sum2 == datetime.datetime(2003, 9, 12, 0, 0, 0, 0)
    assert dt - period_sum2 == datetime.datetime(1997, 3, 8, 0, 0, 0, 0)


def test_last():
    period1 = scate.Period(scate.Unit.YEAR, 1)
    period2 = scate.Period(scate.Unit.YEAR, 2)
    period3 = scate.Period(scate.Unit.MONTH, 3)
    period_sum = scate.Sum([period1, period2, period3])

    year = scate.Year(2000)
    assert scate.Last(year, period1).isoformat() == "1999-01-01T00:00:00 2000-01-01T00:00:00"
    assert scate.Last(year, period_sum).isoformat() == "1996-10-01T00:00:00 2000-01-01T00:00:00"


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


def test_this_p():
    period1 = scate.Period(scate.Unit.YEAR, 1)
    year = scate.Year(2002)
    assert scate.ThisP(year, period1).isoformat() == "2002-01-01T00:00:00 2003-01-01T00:00:00"
    interval = scate.Interval(datetime.datetime(2001, 1, 1), datetime.datetime(2001, 1, 1))
    period2 = scate.Period(scate.Unit.DAY, 5)
    assert scate.ThisP(interval, period2).isoformat() == "2000-12-29T12:00:00 2001-01-03T12:00:00"


def test_repeating_unit():
    interval = scate.Interval.of(2000, 1, 1)
    year = scate.RepeatingUnit(scate.Unit.YEAR)
    assert (interval - year).isoformat() == "1999-01-01T00:00:00 2000-01-01T00:00:00"
    assert (interval - year - year).isoformat() == "1998-01-01T00:00:00 1999-01-01T00:00:00"
    day = scate.RepeatingUnit(scate.Unit.DAY)
    assert (interval + day).isoformat() == "2000-01-02T00:00:00 2000-01-03T00:00:00"
    assert (interval + day + day).isoformat() == "2000-01-03T00:00:00 2000-01-04T00:00:00"


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