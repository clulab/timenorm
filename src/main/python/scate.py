import dataclasses
from dataclasses import field
import datetime
import dateutil.relativedelta as dur
from dateutil.rrule import rrule, YEARLY, MONTHLY, WEEKLY, DAILY, HOURLY, MINUTELY, SECONDLY
from collections.abc import Sequence, Iterator
from enum import Enum


class Unit(Enum):
    MICROSECOND = (None, "MICROSECOND", "SECOND", "microseconds", None, None)
    MILLISECOND = (23, "MILLISECOND", "SECOND", None, None, None)
    SECOND = (19, "SECOND", "MINUTE", "seconds", SECONDLY, "bysecond")
    MINUTE = (16, "MINUTE", "HOUR", "minutes", MINUTELY, "byminute")
    HOUR = (13, "HOUR", "DAY", "hours", HOURLY, "byhour")
    DAY_OF_WEEK = (None, "DAY", "WEEK", "days", DAILY, "byweekday")
    DAY = DAY_OF_MONTH = (10, "DAY", "MONTH", "days", DAILY, "bymonthday")
    DAY_OF_YEAR = (None, "DAY", "YEAR", "days", DAILY, "byyearday")
    WEEK_OF_MONTH = (None, "WEEK", "MONTH", "weeks", None, None)
    WEEK = WEEK_OF_YEAR = (None, "WEEK", "YEAR", "weeks", WEEKLY, "byweekno")
    MONTH = MONTH_OF_YEAR = (None, "MONTH", "YEAR", "months", MONTHLY, "bymonth")
    QUARTER_YEAR = (None, "QUARTER_YEAR", "YEAR", None, None, None)
    YEAR = (None, "YEAR", None, "years", YEARLY, None)
    DECADE = (None, "DECADE", None, None, None, None)
    QUARTER_CENTURY = (None, "QUARTER_CENTURY", None, None, None, None)
    CENTURY = (None, "CENTURY", None, None, None, None)

    def __init__(self, iso_offset, base_name, range_name, relativedelta_name, rrule_freq, rrule_by):
        self.iso_offset = iso_offset
        self.base_name = base_name
        self.range_name = range_name
        self.relativedelta_name = relativedelta_name
        self.rrule_freq = rrule_freq
        self.rrule_by = rrule_by

    @property
    def base(self):
        return Unit[self.base_name]
    
    @property
    def range(self):
        return Unit[self.range_name]

    @staticmethod
    def truncate(ldt: datetime.datetime, unit) -> datetime.datetime:
        if unit == Unit.MICROSECOND:
            return ldt
        elif unit.iso_offset:  # if we can use iso string to truncate
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


@dataclasses.dataclass
class RepeatingInterval:
    base: Unit
    range: Unit


@dataclasses.dataclass
class RepeatingUnit(RepeatingInterval):
    unit: Unit
    base: Unit = field(init=False)
    range: Unit = field(init=False)
    
    def __post_init__(self):
        self.base = self.unit
        self.range = self.unit
    
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
    field: Unit
    value: int
    base: Unit = field(init=False)
    range: Unit = field(init=False)

    def __post_init__(self):
        self.base = self.field.base
        self.range = self.field.range
        self._rrule_kwargs = {"freq": self.range.rrule_freq, self.base.rrule_by: self.value}
        self._one_range = dur.relativedelta(**{self.range.relativedelta_name: 1})
        self._one_base = dur.relativedelta(**{self.base.relativedelta_name: 1})

    def preceding(self, ldt: datetime.datetime) -> Iterator[Interval]:
        while True:
            # rrule requires a starting point even when going backwards,
            # so start at twice the expected range
            ldt = rrule(dtstart=ldt - 2 * self._one_range, **self._rrule_kwargs).before(ldt)
            ldt = Unit.truncate(ldt, self.base)
            yield Interval(ldt, ldt + self._one_base)
    
    def following(self, ldt: datetime.datetime) -> Iterator[Interval]:
        while True:
            ldt = rrule(dtstart=ldt, **self._rrule_kwargs).after(ldt)
            ldt = Unit.truncate(ldt, self.base)
            yield Interval(ldt, ldt + self._one_base)
