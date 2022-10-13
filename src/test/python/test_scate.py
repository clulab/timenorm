import scate


def test_interval():
    year = scate.Interval.of(1985)
    assert year.start.isoformat() == "1985-01-01T00:00:00"
    assert year.end.isoformat() == "1986-01-01T00:00:00"

    year_month = scate.Interval.of(1985, 6)
    assert year_month.start.isoformat() == "1985-06-01T00:00:00"
    assert year_month.end.isoformat() == "1985-07-01T00:00:00"

    year_month_day = scate.Interval.of(1985, 6, 17)
    assert year_month_day.start.isoformat() == "1985-06-17T00:00:00"
    assert year_month_day.end.isoformat() == "1985-06-18T00:00:00"

    year_month_day_hour = scate.Interval.of(1985, 6, 17, 23)
    assert year_month_day_hour.start.isoformat() == "1985-06-17T23:00:00"
    assert year_month_day_hour.end.isoformat() == "1985-06-18T00:00:00"

    year_month_day_hour_minute = scate.Interval.of(1985, 6, 17, 23, 0)
    assert year_month_day_hour_minute.start.isoformat() == "1985-06-17T23:00:00"
    assert year_month_day_hour_minute.end.isoformat() == "1985-06-17T23:01:00"

def test_year():
    year = scate.Year(1985)
    assert(year.start.isoformat() == "1985-01-01T00:00:00")
    assert(year.end.isoformat() == "1986-01-01T00:00:00")

    decade = scate.Year(198, 1)
    assert(decade.start.isoformat() == "1980-01-01T00:00:00")
    assert(decade.end.isoformat() == "1990-01-01T00:00:00")

    century = scate.Year(17, 2)
    assert(century.start.isoformat() == "1700-01-01T00:00:00")
    assert(century.end.isoformat() == "1800-01-01T00:00:00")

def test_year_suffix():
    assert(scate.YearSuffix(scate.Year(1903), 37, 2) == scate.Interval.of(1937))
    assert(scate.YearSuffix(scate.Year(2016), 418, 3) == scate.Interval.of(2418))
    assert(scate.YearSuffix(scate.Year(132, 1), 85, 2) == scate.Interval.of(1385))
    assert(scate.YearSuffix(scate.Year(23, 2), 22, 2) == scate.Interval.of(2322))

    year_plus_1_digit_decade = scate.YearSuffix(scate.Year(1903), 3, 1, 1)
    assert(year_plus_1_digit_decade.start.isoformat() == "1930-01-01T00:00:00")
    assert(year_plus_1_digit_decade.end.isoformat() == "1940-01-01T00:00:00")

    decade_plus_1_digit_decade = scate.YearSuffix(scate.Year(132, 1), 8, 1, 1)
    assert(decade_plus_1_digit_decade.start == "1380-01-01T00:00:00")
    assert(decade_plus_1_digit_decade.end == "1390-01-01T00:00:00")

    decade_plus_3_digit_decade = scate.YearSuffix(scate.Year(132, 1), 240, 3, 1)
    assert(decade_plus_3_digit_decade.start == "2400-01-01T00:00:00")
    assert(decade_plus_3_digit_decade.end == "2410-01-01T00:00:00")
