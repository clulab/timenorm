import dataclasses
from dataclasses import field
import datetime
import dateutil.relativedelta as dur
from collections.abc import Sequence, Iterator
import numpy as np
from time_unit import TimeUnit


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


        

