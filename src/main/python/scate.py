import dataclasses
from dataclasses import field
import datetime
import dateutil.relativedelta as dur
from dateutil.rrule import rrule, YEARLY, MONTHLY, WEEKLY, DAILY, HOURLY, MINUTELY, SECONDLY
from collections.abc import Sequence, Iterator
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
        self.iso_offset = iso_offset
        self.relativedelta_name = relativedelta_name
        self.rrule_freq = rrule_freq

    @staticmethod
    def truncate(ldt: datetime.datetime, unit) -> datetime.datetime:
        if unit.iso_offset > 0:  # if we can use iso string to truncate
            return datetime.datetime.fromisoformat(ldt.isoformat()[:unit.iso_offset])
        else:  # special cases, further calculation/specification needed
            if unit == Unit.CENTURY:
                return datetime.datetime(ldt.year // 100 * 100, 1, 1, 0, 0)
            elif unit == Unit.QUARTER_CENTURY:
                return datetime.datetime(ldt.year // 25 * 25, 1, 1, 0, 0)
            elif unit == Unit.DECADE:
                return datetime.datetime(ldt.year // 10 * 10, 1, 1, 0, 0)
            elif unit == Unit.YEAR:
                return datetime.datetime(ldt.year, 1, 1, 0, 0)
            elif unit == Unit.QUARTER_YEAR:
                return datetime.datetime(ldt.year, (ldt.month - 1) // 3 * 3 + 1, 1, 0, 0)
            elif unit == Unit.MONTH:
                return datetime.datetime(ldt.year, ldt.month, 1, 0, 0)
            elif unit == Unit.WEEK:
                timetuple = ldt.timetuple()
                week_start = datetime.date.fromordinal(timetuple.tm_yday - timetuple.tm_wday)
                return datetime.datetime(ldt.year, week_start.month, week_start.day, 0, 0)


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
        self.rrule_by = rrule_by

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


@dataclasses.dataclass
class Period:
    unit: Unit
    n: int

    def __floordiv__(self, n):
        return Period(self.unit, self.n // n)

    def __truediv__(self, n):
        return Period(self.unit, self.n / n)

    def __radd__(self, other):
        if isinstance(other, datetime.datetime):
            return other + dur.relativedelta(**{self.unit.relativedelta_name: self.n})
        elif isinstance(other, Interval):
            return Interval(other.start + self, other.end + self)
        else:
            raise NotImplementedError

    def __rsub__(self, other):
        if isinstance(other, datetime.datetime):
            return other + dur.relativedelta(**{self.unit.relativedelta_name: -self.n})
        elif isinstance(other, Interval):
            return Interval(other.start - self, other.end - self)
        else:
            raise NotImplementedError

    def expand(self, interval: Interval) -> Interval:
        if interval.start + self > interval.end:
            mid = interval.start + (interval.end - interval.start) / 2
            start = mid - self / 2
            interval = Interval(start, start + self)
        return interval


@dataclasses.dataclass
class ThisP(Interval):
    interval: Interval
    period: Period
    start: datetime.datetime = field(init=False)
    end: datetime.datetime = field(init=False)

    def __post_init__(self):
        self.start, self.end = self.period.expand(self.interval)


class RepeatingInterval:
    def preceding(self, ldt: datetime.datetime) -> Iterator[Interval]:
        raise NotImplementedError

    def following(self, ldt: datetime.datetime) -> Iterator[Interval]:
        raise NotImplementedError


@dataclasses.dataclass
class RepeatingUnit(RepeatingInterval):
    unit: Unit

    def preceding(self, ldt: datetime.datetime) -> Iterator[Interval]:
        one_unit = dur.relativedelta(**{self.unit.relativedelta_name: 1})
        end = Unit.truncate(ldt, self.unit) + one_unit
        start = end - one_unit
        while True:
            end = start
            start = start - one_unit
            yield Interval(start, end)
    
    def following(self, ldt: datetime.datetime) -> Iterator[Interval]:
        one_unit = dur.relativedelta(**{self.unit.relativedelta_name: 1})
        truncated = Unit.truncate(ldt, self.unit)
        end = truncated + one_unit if truncated < ldt else truncated
        while True:
            start = end
            end = start + one_unit
            yield Interval(start, end)


@dataclasses.dataclass
class RepeatingField(RepeatingInterval):
    field: Field
    value: int

    def __post_init__(self):
        self._rrule_kwargs = {"freq": self.field.range.rrule_freq, self.field.rrule_by: self.value}
        self._one_range = dur.relativedelta(**{self.field.range.relativedelta_name: 1})
        self._one_base = dur.relativedelta(**{self.field.base.relativedelta_name: 1})

    def preceding(self, ldt: datetime.datetime) -> Iterator[Interval]:
        while True:
            # rrule requires a starting point even when going backwards,
            # so start at twice the expected range
            ldt = rrule(dtstart=ldt - 2 * self._one_range, **self._rrule_kwargs).before(ldt)
            ldt = Unit.truncate(ldt, self.field.base)
            yield Interval(ldt, ldt + self._one_base)
    
    def following(self, ldt: datetime.datetime) -> Iterator[Interval]:
        while True:
            ldt = rrule(dtstart=ldt, **self._rrule_kwargs).after(ldt)
            ldt = Unit.truncate(ldt, self.field.base)
            yield Interval(ldt, ldt + self._one_base)
