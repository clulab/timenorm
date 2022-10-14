import dataclasses
from dataclasses import field
import datetime
import dateutil.relativedelta as dur
from collections.abc import Sequence
import numpy as np


@dataclasses.dataclass
class Interval:
    start: datetime.datetime
    end: datetime.datetime

    def isoformat(self):
        return f"{self.start.isoformat()}-{self.end.isoformat()}"

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
    trigger_char_span: Sequence[tuple[int, int]] = None
    char_span: Sequence[tuple[int, int]] = field(init = False)
    start: datetime.datetime = field(init = False)
    end: datetime.datetime = field(init = False)
    
    def __post_init__(self):
        self.char_span = max_span(self.interval.char_span, self.trigger_char_span)
        divider = 10 ** (self.n_suffix_digits + self.n_missing_digits)
        multiplier = 10 ** self.n_suffix_digits
        year = Year(int(self.interval.start.year / divider * multiplier + self.last_digits), self.n_missing_digits)
        self.start = year.start
        self.end = year.end

def max_span(interval_span, trigger_span):
    '''
    This is a helper function created to determine, between two spans, which is longer.
    Parameters:
    :interval_span: :trigger_span: tuple[int, int]
    Returns:
    A span (tuple of ints) - either interval_span or trigger_span
    '''
    if not trigger_span:
        return interval_span
    max_span_idx = np.argmax([interval_span[1] - interval_span[0], 
                            trigger_span[1] - trigger_span[0]])
    return interval_span if max_span_idx == 0 else trigger_span
