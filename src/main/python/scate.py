import dataclasses
from dataclasses import field
import datetime
import dateutil.relativedelta as dur
from dateutil.rrule import rrule, YEARLY, MONTHLY, WEEKLY, DAILY, HOURLY, MINUTELY, SECONDLY
from collections.abc import Sequence
from enum import Enum


class Unit(Enum):
    MICROSECOND = (26, "microseconds", None)
    MILLISECOND = (23, None, None)
    SECOND = (19, "seconds", SECONDLY)
    MINUTE = (16, "minutes", MINUTELY)
    HOUR = (13, "hours", HOURLY)
    DAY = (10, "days", DAILY)
    # assign unique offsets so tuples are distinct
    WEEK = (-1, "weeks", WEEKLY)
    MONTH = (-2, "months", MONTHLY)
    QUARTER_YEAR = (-3, None, None)
    YEAR = (-4, "years", YEARLY)
    DECADE = (-5, None, None)
    QUARTER_CENTURY = (-6, None, None)
    CENTURY = (-7, None, None)

    def __init__(self, iso_offset, relativedelta_name, rrule_freq):
        self._iso_offset = iso_offset
        self._relativedelta_name = relativedelta_name
        self._rrule_freq = rrule_freq

    def truncate(self, dt: datetime.datetime) -> datetime.datetime:
        if self._iso_offset > 0:  # if we can use iso string to truncate
            return datetime.datetime.fromisoformat(dt.isoformat()[:self._iso_offset])
        else:  # special cases, further calculation/specification needed
            if self == Unit.CENTURY:
                return datetime.datetime(dt.year // 100 * 100, 1, 1, 0, 0)
            elif self == Unit.QUARTER_CENTURY:
                return datetime.datetime(dt.year // 25 * 25, 1, 1, 0, 0)
            elif self == Unit.DECADE:
                return datetime.datetime(dt.year // 10 * 10, 1, 1, 0, 0)
            elif self == Unit.YEAR:
                return datetime.datetime(dt.year, 1, 1, 0, 0)
            elif self == Unit.QUARTER_YEAR:
                return datetime.datetime(dt.year, (dt.month - 1) // 3 * 3 + 1, 1, 0, 0)
            elif self == Unit.MONTH:
                return datetime.datetime(dt.year, dt.month, 1, 0, 0)
            elif self == Unit.WEEK:
                timetuple = dt.timetuple()
                week_start = datetime.date.fromordinal(timetuple.tm_yday - timetuple.tm_wday)
                return datetime.datetime(dt.year, week_start.month, week_start.day, 0, 0)

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
        if interval.start + self.unit.relativedelta(self.n) > interval.end:
            mid = interval.start + (interval.end - interval.start) / 2
            start = mid - self.unit.relativedelta(self.n / 2)
            interval = Interval(start, start + self)
        return interval


@dataclasses.dataclass
class Sum(Offset):
    periods: list[Period]

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
class ThisP(Interval):
    interval: Interval
    period: Period
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)

    def __post_init__(self):
        self.start, self.end = self.period.expand(self.interval)
