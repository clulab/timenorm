import dataclasses
from dataclasses import field
import datetime
import dateutil.relativedelta as dur
from dateutil.rrule import rrule, YEARLY, MONTHLY, WEEKLY, DAILY, HOURLY, MINUTELY, SECONDLY
from collections.abc import Sequence, Iterator
from collections import namedtuple
import numpy as np
from enum import Enum



class TimeUnit(Enum):
    # values of units are (ISO offset, base name, range name)
    # base name is required due to presence of units such as "DAY_OF_MONTH" where base needs to be specified.
    MICROSECOND = (None, "MICROSECOND","SECOND", None, None)
    MILLISECOND = (23, "MILLISECOND","SECOND", None, None)
    SECOND = (19, "SECOND","MINUTE", SECONDLY, "bysecond")
    MINUTE = (16, "MINUTE","HOUR", MINUTELY, "byminute")
    HOUR = (13, "HOUR","DAY", HOURLY, "byhour")
    DAY_OF_WEEK = (None, "DAY", "WEEK", DAILY, "byweekday")
    DAY_OF_MONTH = (10, "DAY", "MONTH", DAILY, "bymonthday")
    DAY_OF_YEAR = (None, "DAY", "YEAR", DAILY, "byyearday")
    DAY = tuple(DAY_OF_MONTH)
    WEEK_OF_MONTH = (None, "WEEK", "MONTH", None, None)
    WEEK_OF_YEAR = (None, "WEEK", "YEAR", WEEKLY, "byweekno")
    WEEK = tuple(WEEK_OF_YEAR)
    MONTH_OF_YEAR = (None, "MONTH", "YEAR", MONTHLY, "bymonth")
    MONTH = tuple(MONTH_OF_YEAR)
    QUARTER_YEAR = (None, "QUARTER_YEAR", "YEAR", None, None)
    YEAR = (None, "YEAR", None, YEARLY, None)
    DECADE = (None, "DECADE", None, None, None)
    QUARTER_CENTURY = (None, "QUARTER_CENTURY", None, None, None)
    CENTURY = (None, "CENTURY", None, None, None)

    def __init__(self, iso_offset, base_name, range_name, rrule_freq, rrule_by):
        self.iso_offset = iso_offset
        self.base_name = base_name
        self.range_name = range_name
        self.rrule_freq = rrule_freq
        self.rrule_by = rrule_by

    @property
    def base(self):
        return TimeUnit[self.base_name]
    
    @property
    def range(self):
        return TimeUnit[self.range_name]
    
    @property
    def dt_format(self):
        if self.name in ["MONTH_OF_YEAR", "DAY_OF_MONTH"]:
            return self.name.split("_OF_")[0].lower()
        elif self.name not in ["YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECOND", "MICROSECOND"]:
            raise ValueError(f"{self.name} unit type does not exist for datetime objects")
        return self.name.lower()
    
    @property
    def rd_format(self):
        if self.name in ["MONTH_OF_YEAR", "DAY_OF_MONTH"]:
            return self.dt_format + "s"
        if self.name not in ["YEAR", "MONTH", "WEEK", "DAY", "HOUR", "MINUTE", "SECOND", "MICROSECOND"]:
            raise ValueError(f"{self.name} unit type does not exist in dateutil.relativedelta")
        return self.dt_format + "s"

    @staticmethod
    def truncate(ldt: datetime.datetime, unit) -> datetime.datetime:
        if unit == TimeUnit.MICROSECOND:
            return ldt
        elif unit.iso_offset: # if we can use iso string to truncate
            return datetime.datetime.fromisoformat(ldt.isoformat()[:unit.iso_offset])
        else: # special cases, further calculation/specification needed
            if unit == TimeUnit.CENTURY:
                return datetime.datetime(ldt.year // 100 * 100, 1, 1, 0, 0)
            elif unit == TimeUnit.QUARTER_CENTURY:
                return datetime.datetime(ldt.year // 25 * 25, 1, 1, 0, 0)
            elif unit == TimeUnit.DECADE:
                return datetime.datetime(ldt.year // 10 * 10, 1, 1, 0, 0)
            elif unit == TimeUnit.YEAR:
                return datetime.datetime(ldt.year, 1, 1, 0, 0)
            elif unit == TimeUnit.QUARTER_YEAR:
                return datetime.datetime(ldt.year, (ldt.month - 1) // 3 * 3 + 1, 1, 0, 0)
            elif unit == TimeUnit.MONTH: # shouldn't MONTH_OF_YEAR be the same?
                return datetime.datetime(ldt.year, ldt.month, 1, 0, 0)
            elif unit == TimeUnit.WEEK:
                week_start = datetime.date.fromordinal(ldt.timetuple().tm_yday - ldt.timetuple().tm_wday)
                return datetime.datetime(ldt.year, week_start.month, week_start.day, 0, 0)

@dataclasses.dataclass
class Interval:
    start: datetime.datetime
    end: datetime.datetime
    
    def isoformat(self):
        return f"{self.start.isoformat()} {self.end.isoformat()}"

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
        # end is one smallest unit specified larger than the start, and relativedelta argument names are plural
        last_name, _ = pairs[-1]
        last_name += "s"
        end = start + dur.relativedelta(**{last_name: 1})
        return cls(start, end)

@dataclasses.dataclass
class Year(Interval):
    digits: int
    n_missing_digits: int = 0
    char_span: Sequence[tuple[int, int]] = None
    start: datetime.datetime = field(init = False)
    end: datetime.datetime = field(init = False)
    # create start and end instance variables using passed in values.
    def __post_init__(self):
        duration_in_years = 10 ** self.n_missing_digits
        self.start = datetime.datetime(year = self.digits * duration_in_years,
                                       month = 1,
                                       day = 1)
        self.end = self.start + dur.relativedelta(years = duration_in_years)

@dataclasses.dataclass
class YearSuffix(Interval):
    interval: Interval
    last_digits: int
    n_suffix_digits: int
    n_missing_digits: int = 0
    start: datetime.datetime = field(init = False)
    end: datetime.datetime = field(init = False)
    
    def __post_init__(self):
        divider = int(10 ** (self.n_suffix_digits + self.n_missing_digits))
        multiplier = int(10 ** self.n_suffix_digits)
        year = Year(self.interval.start.year // divider * multiplier + self.last_digits, self.n_missing_digits)
        self.start = year.start
        self.end = year.end

@dataclasses.dataclass
class Period: # extend relative delta
    unit: TimeUnit
    n: int
    def __post_init__(self):
        if type(self.n) != int:
            raise TypeError(self.__repr__ + "does not support" + str(type(self.n)))
    # try to use relative delta instead of add_to and subtract_from
    def add_to(self, date: datetime.datetime): # way to override __add__ instead? __add__, __radd__
        return date + dur.relativedelta(**{self.unit.rd_format: self.n})

    def subtract_from(self, date: datetime.datetime):
        return date - dur.relativedelta(**{self.unit.rd_format: self.n})
    
    @staticmethod
    def expand(interval: Interval, period) -> Interval:
        mid = interval.start + (interval.end - interval.start) / 2
        half_period = (interval.start - period.subtract_from(interval.start)) / 2
        start = mid - half_period
        return Interval(start, period.add_to(start))
    
    #### VERSION OF .expand without add_to and subtract_from methods... not as clear?
    # @staticmethod
    # def expand2(interval: Interval, period) -> Interval:
    #     mid = interval.start + (interval.end - interval.start) / 2
    #     half_period = (interval.start - (interval.start - dur.relativedelta(**{period.unit.rd_format: period.n}))) / 2
    #     start = mid - half_period
    #     return Interval(start, period.add_to(start))

    @staticmethod
    def one_unit(period):
        return Period(period.unit, 1)

    @staticmethod
    def expand_if_larger(interval: Interval, period) -> Interval:
        # if the end date of interval is larger than the start increased by the period
        # return the interval
        return Period.expand(interval, period) if interval.end >= period.add_to(interval.start) else interval

@dataclasses.dataclass
class ThisP(Interval):
    interval: Interval
    period: Period
    start: datetime.datetime = field(init = False)
    end:datetime.datetime = field(init = False)

    def __post_init__(self):
        this_period = Period.expand(self.interval, self.period)
        self.start = this_period.start
        self.end = this_period.end

@dataclasses.dataclass
class RepeatingInterval:
    base: TimeUnit # unit
    range: TimeUnit # unit

@dataclasses.dataclass
class RepeatingUnit(RepeatingInterval):
    unit: TimeUnit
    base: TimeUnit = field(init = False)
    range: TimeUnit = field(init = False)
    
    def __post_init__(self):
        self.base = self.unit
        self.range = self.unit
    
    def preceding(self, ldt: datetime.datetime) -> Iterator[Interval]:
        end = TimeUnit.truncate(ldt, self.unit) + dur.relativedelta(**{self.unit.rd_format: 1})
        start = end - dur.relativedelta(**{self.unit.rd_format:1})
        while True:
            end = start
            start = start - dur.relativedelta(**{self.unit.rd_format:1})
            yield Interval(start, end)
    
    def following(self, ldt: datetime.datetime) -> Iterator[Interval]:
        truncated = TimeUnit.truncate(ldt, self.unit)
        end = truncated + dur.relativedelta(**{self.unit.rd_format:1}) if truncated < ldt else truncated
        start = end - dur.relativedelta(**{self.unit.rd_format:1})
        while True:
            start = end
            end = start + dur.relativedelta(**{self.unit.rd_format:1})
            yield Interval(start, end)

@dataclasses.dataclass
class RepeatingField(RepeatingInterval):
    field: TimeUnit
    value: int
    base: TimeUnit = field(init = False)
    range: TimeUnit = field(init = False)

    def __post_init__(self):
        self.base = self.field.base
        self.range = self.field.range
        self._rrule_kwargs = {"freq": self.range.rrule_freq, self.base.rrule_by: self.value}
        self._one_range = dur.relativedelta(**{self.range.rd_format: 1})
        self._one_base = dur.relativedelta(**{self.base.rd_format: 1})

    def preceding(self, ldt: datetime.datetime) -> Iterator[Interval]:
        while True:
            # rrule requires a starting point even when going backwards, so start twice the expected range
            ldt = rrule(dtstart=ldt - 2 * self._one_range, **self._rrule_kwargs).before(ldt)
            ldt = TimeUnit.truncate(ldt, self.base)
            yield Interval(ldt, ldt + self._one_base)
    
    def following(self, ldt: datetime.datetime) -> Iterator[Interval]:
        while True:
            ldt = rrule(dtstart=ldt, **self._rrule_kwargs).after(ldt)
            ldt = TimeUnit.truncate(ldt, self.base)
            yield Interval(ldt, ldt + self._one_base)



        

