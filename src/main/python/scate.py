import dataclasses
from dataclasses import field
import datetime
import dateutil.relativedelta as dur
from dateutil.rrule import rrule, YEARLY, MONTHLY, WEEKLY, DAILY, HOURLY, MINUTELY, SECONDLY
from collections.abc import Sequence
from enum import Enum


@dataclasses.dataclass
class Interval:
    start: datetime.datetime
    end: datetime.datetime

    def isoformat(self):
        return f"{self.start.isoformat()} {self.end.isoformat()}"

    def __len__(self):
        return 2

    def __iter__(self):
        yield self.start
        yield self.end

    @classmethod
    def of(cls, year, *args):
        # match Interval.of arguments with datetime.__init__ arguments
        names = ["year", "month", "day", "hour", "minute", "second", "microsecond"]
        pairs = list(zip(names, (year,) + args))
        kwargs = dict(pairs)
        # month and day are required by datetime, so give defaults here
        for name in ["month", "day"]:
            if name not in kwargs:
                kwargs[name] = 1
        # create the datetime for the start
        start = datetime.datetime(**kwargs)
        # end is one smallest unit specified larger than the start
        last_name, _ = pairs[-1]
        # relativedelta argument names are plural
        last_name += "s"
        end = start + dur.relativedelta(**{last_name: 1})
        return cls(start, end)


class Unit(Enum):
    MICROSECOND = (1, "microseconds", None)
    MILLISECOND = (2, None, None)
    SECOND = (3, "seconds", SECONDLY)
    MINUTE = (4, "minutes", MINUTELY)
    HOUR = (5, "hours", HOURLY)
    DAY = (6, "days", DAILY)
    WEEK = (7, "weeks", WEEKLY)
    MONTH = (8, "months", MONTHLY)
    QUARTER_YEAR = (9, None, None)
    YEAR = (10, "years", YEARLY)
    DECADE = (11, None, None)
    QUARTER_CENTURY = (12, None, None)
    CENTURY = (13, None, None)

    def __init__(self, n, relativedelta_name, rrule_freq):
        self._n = n
        self._relativedelta_name = relativedelta_name
        self._rrule_freq = rrule_freq

    def truncate(self, dt: datetime.datetime) -> datetime.datetime:
        if self is Unit.MILLISECOND:
            dt = dt.replace(microsecond=dt.microsecond // 1000 * 1000)
        elif self is Unit.SECOND:
            dt = dt.replace(microsecond=0)
        elif self is Unit.MINUTE:
            dt = dt.replace(second=0, microsecond=0)
        elif self is Unit.HOUR:
            dt = dt.replace(minute=0, second=0, microsecond=0)
        elif self is Unit.DAY:
            dt = dt.replace(hour=0, minute=0, second=0, microsecond=0)
        elif self is Unit.WEEK:
            timetuple = dt.timetuple()
            week_start = datetime.date.fromordinal(timetuple.tm_yday - timetuple.tm_wday)
            dt = datetime.datetime(dt.year, week_start.month, week_start.day, 0, 0)
        elif self is Unit.MONTH:
            dt = datetime.datetime(dt.year, dt.month, 1, 0, 0)
        elif self is Unit.QUARTER_YEAR:
            dt = datetime.datetime(dt.year, (dt.month - 1) // 3 * 3 + 1, 1, 0, 0)
        elif self is Unit.YEAR:
            dt = datetime.datetime(dt.year, 1, 1, 0, 0)
        elif self is Unit.DECADE:
            dt = datetime.datetime(dt.year // 10 * 10, 1, 1, 0, 0)
        elif self is Unit.QUARTER_CENTURY:
            dt = datetime.datetime(dt.year // 25 * 25, 1, 1, 0, 0)
        elif self is Unit.CENTURY:
            dt = datetime.datetime(dt.year // 100 * 100, 1, 1, 0, 0)
        return dt

    def relativedelta(self, n):
        if self._relativedelta_name is not None:
            return dur.relativedelta(**{self._relativedelta_name: n})
        else:
            raise NotImplementedError

    def rrule_kwargs(self):
        if self._rrule_freq is not None:
            return {"freq": self._rrule_freq}
        else:
            raise NotImplementedError

    def expand(self, interval: Interval, n: int = 1) -> Interval:
        if interval.start + self.relativedelta(n) > interval.end:
            mid = interval.start + (interval.end - interval.start) / 2
            if n % 2 == 0 or self in {Unit.MILLISECOND,
                                      Unit.MICROSECOND,
                                      Unit.SECOND,
                                      Unit.MINUTE,
                                      Unit.HOUR,
                                      Unit.DAY,
                                      Unit.WEEK}:
                half = self.relativedelta(n / 2)
            elif self is Unit.MONTH:
                half = Unit.DAY.relativedelta(30 / 2)
            elif self is Unit.YEAR:
                half = Unit.DAY.relativedelta(365 / 2)
            else:
                raise NotImplementedError(f"don't know how to take {n}/2 {self}")
            start = mid - half
            interval = Interval(start, start + self.relativedelta(n))
        return interval


class Field(Enum):
    DAY_OF_WEEK = (Unit.DAY, Unit.WEEK, "byweekday")
    DAY_OF_MONTH = (Unit.DAY, Unit.MONTH, "bymonthday")
    DAY_OF_YEAR = (Unit.DAY, Unit.YEAR, "byyearday")
    WEEK_OF_MONTH = (Unit.WEEK, Unit.MONTH, None)
    WEEK_OF_YEAR = (Unit.WEEK, Unit.YEAR, "byweekno")
    MONTH_OF_YEAR = (Unit.MONTH, Unit.YEAR, "bymonth")
    QUARTER_OF_YEAR = (Unit.QUARTER_YEAR, Unit.YEAR, None)

    def __init__(self, base, range, rrule_by):
        self.base = base
        self.range = range
        self._rrule_by = rrule_by

    def rrule_kwargs(self, value):
        if self._rrule_by is not None:
            return {self._rrule_by: value}
        else:
            raise NotImplementedError


@dataclasses.dataclass
class Year(Interval):
    digits: int
    n_missing_digits: int = 0
    char_span: Sequence[tuple[int, int]] = None
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)

    def __post_init__(self):
        duration_in_years = 10 ** self.n_missing_digits
        self.start = datetime.datetime(year=self.digits * duration_in_years,
                                       month=1,
                                       day=1)
        self.end = self.start + dur.relativedelta(years=duration_in_years)


@dataclasses.dataclass
class YearSuffix(Interval):
    interval: Interval
    last_digits: int
    n_suffix_digits: int
    n_missing_digits: int = 0
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)
    
    def __post_init__(self):
        divider = int(10 ** (self.n_suffix_digits + self.n_missing_digits))
        multiplier = int(10 ** self.n_suffix_digits)
        truncated_year = self.interval.start.year // divider
        year = Year(truncated_year * multiplier + self.last_digits, self.n_missing_digits)
        self.start = year.start
        self.end = year.end


class Offset:
    unit: Unit

    def __rsub__(self, other: Interval) -> Interval:
        raise NotImplementedError

    def __radd__(self, other: Interval) -> Interval:
        raise NotImplementedError


@dataclasses.dataclass
class Period(Offset):
    unit: Unit
    n: int

    def __radd__(self, other):
        if isinstance(other, datetime.datetime):
            return other + self.unit.relativedelta(self.n)
        elif isinstance(other, Interval):
            return Interval(other.start + self, other.end + self)
        else:
            raise NotImplementedError

    def __rsub__(self, other):
        if isinstance(other, datetime.datetime):
            return other - self.unit.relativedelta(self.n)
        elif isinstance(other, Interval):
            return Interval(other.start - self, other.end - self)
        else:
            raise NotImplementedError

    def expand(self, interval: Interval) -> Interval:
        return self.unit.expand(interval, self.n)


@dataclasses.dataclass
class Sum(Offset):
    periods: list[Period]

    def __post_init__(self):
        self.unit = max(self.periods, key=lambda p: p.unit._n).unit

    def __radd__(self, other):
        for period in self.periods:
            other += period
        return other

    def __rsub__(self, other):
        for period in self.periods:
            other -= period
        return other


@dataclasses.dataclass
class RepeatingUnit(Offset):
    unit: Unit

    def __rsub__(self, interval: Interval) -> Interval:
        one_unit = self.unit.relativedelta(1)
        end = self.unit.truncate(interval.start) + one_unit
        start = end - one_unit
        end = start
        start = start - one_unit
        return Interval(start, end)
    
    def __radd__(self, interval: Interval) -> Interval:
        one_unit = self.unit.relativedelta(1)
        truncated = self.unit.truncate(interval.end)
        end = truncated + one_unit if truncated < interval.end else truncated
        start = end
        end = start + one_unit
        return Interval(start, end)


@dataclasses.dataclass
class RepeatingField(Offset):
    field: Field
    value: int

    def __post_init__(self):
        self.unit = self.field.base
        self._rrule_kwargs = {}
        self._rrule_kwargs.update(self.field.range.rrule_kwargs())
        self._rrule_kwargs.update(self.field.rrule_kwargs(self.value))
        self._one_range = self.field.range.relativedelta(1)
        self._one_base = self.field.base.relativedelta(1)

    def __rsub__(self, interval: Interval) -> Interval:
        # rrule requires a starting point even when going backwards,
        # so start at twice the expected range
        dtstart = interval.start - 2 * self._one_range
        ldt = rrule(dtstart=dtstart, **self._rrule_kwargs).before(interval.start)
        ldt = self.field.base.truncate(ldt)
        return Interval(ldt, ldt + self._one_base)
    
    def __radd__(self, interval: Interval) -> Interval:
        ldt = rrule(dtstart=interval.end, **self._rrule_kwargs).after(interval.end)
        ldt = self.field.base.truncate(ldt)
        return Interval(ldt, ldt + self._one_base)


@dataclasses.dataclass
class Last(Interval):
    interval: Interval
    offset: Offset
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)

    def __post_init__(self):
        self.end = self.interval.start
        self.start = self.interval.start - self.offset


@dataclasses.dataclass
class Next(Interval):
    interval: Interval
    offset: Offset
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)

    def __post_init__(self):
        self.start = self.interval.end
        self.end = self.interval.end + self.offset


@dataclasses.dataclass
class Before(Interval):
    interval: Interval
    offset: Offset
    expand: bool = False
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)

    def __post_init__(self):
        interval = self.interval - self.offset
        if self.expand:
            interval = self.offset.unit.expand(interval)
        self.start, self.end = interval


@dataclasses.dataclass
class After(Interval):
    interval: Interval
    offset: Offset
    expand: bool = False
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)

    def __post_init__(self):
        interval = self.interval + self.offset
        if self.expand:
            interval = self.offset.unit.expand(interval)
        self.start, self.end = interval


@dataclasses.dataclass
class This(Interval):
    interval: Interval
    period: Period
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)

    def __post_init__(self):
        self.start, self.end = self.period.expand(self.interval)
