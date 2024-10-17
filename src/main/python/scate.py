import abc
import argparse
import collections
import dataclasses
import datetime
import sys

import dateutil.relativedelta
import dateutil.rrule
import enum
import pathlib
import re
import sys
import traceback
import typing
import xml.etree.ElementTree as ET


def _dataclass(cls):
    cls = dataclasses.dataclass(cls, repr=False)

    def __repr__(self):
        fields = [f for f in dataclasses.fields(self)
                  if f.repr
                  and getattr(self, f.name) != f.default
                  and (f.default_factory is dataclasses.MISSING
                       or getattr(self, f.name) != f.default_factory())]
        field_str = ', '.join([f"{f.name}={getattr(self, f.name)!r}" for f in fields])
        return f"{self.__class__.__qualname__}({field_str})"

    cls.__repr__ = __repr__
    return cls


@dataclasses.dataclass
class Interval:
    start: datetime.datetime | None
    end: datetime.datetime | None

    def is_defined(self) -> bool:
        return self.start is not None and self.end is not None

    def isoformat(self) -> str:
        start_str = "..." if self.start is None else self.start.isoformat()
        end_str = "..." if self.end is None else self.end.isoformat()
        return f"{start_str} {end_str}"

    def __repr__(self):
        delta = dateutil.relativedelta.relativedelta(self.end, self.start)
        if delta == dateutil.relativedelta.relativedelta(years=+1):
            tuple_index = 1
        elif delta == dateutil.relativedelta.relativedelta(months=+1):
            tuple_index = 2
        elif delta == dateutil.relativedelta.relativedelta(days=+1):
            tuple_index = 3
        elif delta == dateutil.relativedelta.relativedelta(hours=+1):
            tuple_index = 4
        elif delta == dateutil.relativedelta.relativedelta(minutes=+1):
            tuple_index = 5
        elif delta == dateutil.relativedelta.relativedelta(seconds=+1):
            tuple_index = 6
        else:
            tuple_index = None
        if tuple_index is not None:
            return f"Interval.of({', '.join(map(repr, self.start.timetuple()[:tuple_index]))})"
        else:
            return f"Interval(start={self.start!r}, end={self.end!r})"

    def __len__(self):
        return 2

    def __iter__(self):
        yield self.start
        yield self.end

    def __add__(self, offset):
        return self.end + offset

    def __sub__(self, offset):
        return self.start - offset

    @classmethod
    def fromisoformat(cls, string):
        start, end = [datetime.datetime.fromisoformat(x) for x in string.split()]
        return cls(start, end)

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
        end = start + dateutil.relativedelta.relativedelta(**{last_name: 1})
        return cls(start, end)


class Unit(enum.Enum):
    MICROSECOND = (1, "microseconds")
    MILLISECOND = (2, None)
    SECOND = (3, "seconds")
    MINUTE = (4, "minutes")
    HOUR = (5, "hours")
    DAY = (6, "days")
    WEEK = (7, "weeks")
    MONTH = (8, "months")
    QUARTER_YEAR = (9, None)
    YEAR = (10, "years")
    DECADE = (11, None)
    QUARTER_CENTURY = (12, None)
    CENTURY = (13, None)

    def __init__(self, n, relativedelta_name):
        self._n = n
        self._relativedelta_name = relativedelta_name

    def __lt__(self, other) -> bool:
        if self.__class__ is other.__class__:
            return self._n < other._n
        return NotImplemented

    def __repr__(self):
        return self.name

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
            diff = timetuple.tm_yday - timetuple.tm_wday
            week_start = datetime.date.fromordinal(diff if diff >= 1 else diff + 7)
            dt = datetime.datetime(dt.year, week_start.month, week_start.day, 0, 0)
            if diff < 1:
                dt = dt + dateutil.relativedelta.relativedelta(days=-7)
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
            year = dt.year // 100 * 100
            year = 1 if year == 0 else year  # year 0 does not exist
            dt = datetime.datetime(year, 1, 1, 0, 0)
        return dt

    def relativedelta(self, n) -> dateutil.relativedelta.relativedelta:
        if self._relativedelta_name is not None:
            return dateutil.relativedelta.relativedelta(**{self._relativedelta_name: n})
        elif self is Unit.CENTURY:
            return dateutil.relativedelta.relativedelta(**{Unit.YEAR._relativedelta_name: 100 * n})
        elif self is Unit.QUARTER_CENTURY:
            return dateutil.relativedelta.relativedelta(**{Unit.YEAR._relativedelta_name: 25 * n})
        elif self is Unit.DECADE:
            return dateutil.relativedelta.relativedelta(**{Unit.YEAR._relativedelta_name: 10 * n})
        elif self is Unit.QUARTER_YEAR:
            return dateutil.relativedelta.relativedelta(**{Unit.MONTH._relativedelta_name: 3 * n})
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


# allow e.g., scate.DAY instead of scate.Unit.DAY
globals().update(Unit.__members__)


class Offset:
    unit: Unit

    def __rsub__(self, other: datetime.datetime) -> Interval:
        raise NotImplementedError

    def __radd__(self, other: datetime.datetime) -> Interval:
        raise NotImplementedError


@_dataclass
class Period(Offset):
    unit: Unit
    n: int | None
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __radd__(self, other: datetime.datetime) -> Interval:
        if self.unit is None or self.n is None:
            return Interval(other, None)
        else:
            end = other + self.unit.relativedelta(self.n)
            # in the first century, there's only 99 years
            if other == datetime.datetime.min and self.unit is Unit.CENTURY:
                end -= Unit.YEAR.relativedelta(1)
            return Interval(other, end)

    def __rsub__(self, other: datetime.datetime) -> Interval:
        if self.unit is None or self.n is None:
            return Interval(None, other)
        else:
            return Interval(other - self.unit.relativedelta(self.n), other)

    def expand(self, interval: Interval) -> Interval:
        if self.unit is None or self.n is None:
            return Interval(None, None)
        else:
            return self.unit.expand(interval, self.n)


@_dataclass
class PeriodSum(Offset):
    periods: list[Period]
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        self.unit = max(self.periods, key=lambda p: p.unit).unit

    def __radd__(self, other: datetime.datetime) -> Interval:
        end = other
        for period in self.periods:
            end = (end + period).end
        return Interval(other, end)

    def __rsub__(self, other: datetime.datetime) -> Interval:
        start = other
        for period in self.periods:
            start = (start - period).start
        return Interval(start, other)


@_dataclass
class Repeating(Offset):
    unit: Unit
    range: Unit = None
    value: int = dataclasses.field(default=None, kw_only=True)
    n_units: int = dataclasses.field(default=1, kw_only=True)
    rrule_kwargs: dict = dataclasses.field(default_factory=dict, kw_only=True, repr=False)
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        self.period = Period(self.unit, self.n_units)
        if self.range == self.unit:
            pass  # same as self.range is None
        elif self.range is None:
            self.range = self.unit
        elif self.value is None:
            raise ValueError(f"value=None is not allowed for unit={self.unit} and range={self.range}")
        else:
            match self.range:
                case Unit.SECOND:
                    rrule_freq = dateutil.rrule.SECONDLY
                case Unit.MINUTE:
                    rrule_freq = dateutil.rrule.MINUTELY
                case Unit.HOUR:
                    rrule_freq = dateutil.rrule.HOURLY
                case Unit.DAY:
                    rrule_freq = dateutil.rrule.DAILY
                case Unit.WEEK:
                    rrule_freq = dateutil.rrule.WEEKLY
                case Unit.MONTH:
                    rrule_freq = dateutil.rrule.MONTHLY
                case Unit.YEAR:
                    rrule_freq = dateutil.rrule.YEARLY
                case _:
                    raise NotImplementedError
            self.rrule_kwargs["freq"] = rrule_freq

            match (self.unit, self.range):
                case (Unit.SECOND, Unit.MINUTE):
                    rrule_by = "bysecond"
                case (Unit.MINUTE, Unit.HOUR):
                    rrule_by = "byminute"
                case (Unit.HOUR, Unit.DAY):
                    rrule_by = "byhour"
                case (Unit.DAY, Unit.WEEK):
                    rrule_by = "byweekday"
                case (Unit.DAY, Unit.MONTH):
                    rrule_by = "bymonthday"
                case (Unit.DAY, Unit.YEAR):
                    rrule_by = "byyearday"
                case (Unit.WEEK, Unit.YEAR):
                    rrule_by = "byweekno"
                case (Unit.MONTH, Unit.YEAR):
                    rrule_by = "bymonth"
                case _:
                    raise NotImplementedError
            self.rrule_kwargs[rrule_by] = self.value

    def __rsub__(self, other: datetime.datetime) -> Interval:
        if self.unit is None:
            return Interval(None, None)
        other = self.unit.truncate(other)
        if self.rrule_kwargs:
            # HACK: rrule requires a starting point even when going backwards so use a big one
            dtstart = other - Unit.YEAR.relativedelta(100)
            min_end = other - self.period.unit.relativedelta(self.period.n)
            start = dateutil.rrule.rrule(dtstart=dtstart, **self.rrule_kwargs).before(min_end, inc=True)
            if start is None:
                raise ValueError(f"between {dtstart} and {min_end} there is no {self.rrule_kwargs}")
            interval = start + self.period
        else:
            interval = other - self.period
        return interval

    def __radd__(self, other: datetime.datetime) -> Interval:
        if self.unit is None:
            return Interval(None, None)
        start = self.unit.truncate(other)
        if self.rrule_kwargs:
            start = dateutil.rrule.rrule(dtstart=start, **self.rrule_kwargs).after(other)
        elif start < other:
            start += self.period.unit.relativedelta(1)
        return start + self.period


# Defined as "meterological seasons"
# https://www.ncei.noaa.gov/news/meteorological-versus-astronomical-seasons
@_dataclass
class Spring(Repeating):
    unit: Unit = Unit.MONTH
    range: Unit = Unit.YEAR
    value: int = 3
    n_units: int = 3


@_dataclass
class Summer(Repeating):
    unit: Unit = Unit.MONTH
    range: Unit = Unit.YEAR
    value: int = 6
    n_units: int = 3


@_dataclass
class Fall(Repeating):
    unit: Unit = Unit.MONTH
    range: Unit = Unit.YEAR
    value: int = 9
    n_units: int = 3


@_dataclass
class Winter(Repeating):
    unit: Unit = Unit.MONTH
    range: Unit = Unit.YEAR
    value: int = 12
    n_units: int = 3


@_dataclass
class Weekend(Repeating):
    unit: Unit = Unit.DAY
    n_units: int = 2
    rrule_kwargs: dict = dataclasses.field(
        default_factory=lambda: dict(freq=dateutil.rrule.DAILY, byweekday=5))


# defined as used in forecasts
# https://www.weather.gov/bgm/forecast_terms
@_dataclass
class Morning(Repeating):
    unit: Unit = Unit.HOUR
    range: Unit = Unit.DAY
    value: int = 6
    n_units: int = 6


@_dataclass
class Noon(Repeating):
    unit: Unit = Unit.MINUTE
    rrule_kwargs: dict = dataclasses.field(
        default_factory=lambda: dict(freq=dateutil.rrule.DAILY, byhour=12, byminute=0))


@_dataclass
class Afternoon(Repeating):
    unit: Unit = Unit.HOUR
    range: Unit = Unit.DAY
    value: int = 12
    n_units: int = 6


@_dataclass
class Day(Repeating):
    unit: Unit = Unit.HOUR
    range: Unit = Unit.DAY
    value: int = 6
    n_units: int = 12


@_dataclass
class Evening(Repeating):
    unit: Unit = Unit.HOUR
    range: Unit = Unit.DAY
    value: int = 18
    n_units: int = 6


@_dataclass
class Night(Repeating):
    unit: Unit = Unit.HOUR
    range: Unit = Unit.DAY
    value: int = 0
    n_units: int = 6


@_dataclass
class Midnight(Repeating):
    unit: Unit = Unit.MINUTE
    rrule_kwargs: dict = dataclasses.field(
        default_factory=lambda: dict(freq=dateutil.rrule.DAILY, byhour=0, byminute=0))


@_dataclass
class EveryNth(Offset):
    offset: Offset
    n: int
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __rsub__(self, other: datetime.datetime) -> Interval:
        interval = other - self.offset
        for _ in range(self.n - 1):
            interval -= self.offset
        return interval

    def __radd__(self, other: datetime.datetime) -> Interval:
        interval = other + self.offset
        for _ in range(self.n - 1):
            interval += self.offset
        return interval


@_dataclass
class OffsetUnion(Offset):
    offsets: typing.Iterable[Offset]
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        self.unit = min(o.unit for o in self.offsets)
        self.range = max(o.unit for o in self.offsets)

    def __rsub__(self, other: datetime.datetime) -> Interval:
        return max((other - offset for offset in self.offsets),
                   key=lambda i: (i.end, i.end - i.start))

    def __radd__(self, other: datetime.datetime) -> Interval:
        return min((other + offset for offset in self.offsets),
                   key=lambda i: (i.start, i.start - i.end))


@_dataclass
class RepeatingIntersection(Offset):
    offsets: typing.Iterable[Repeating]
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def _iter_offsets(self) -> typing.Iterator[Repeating]:
        for offset in self.offsets:
            if isinstance(offset, RepeatingIntersection):
                yield from offset._iter_offsets()
            elif not isinstance(offset, Repeating):
                raise NotImplementedError(offset)
            else:
                yield offset

    def __post_init__(self):
        if not self.offsets:
            raise ValueError(f"{self.__class__.__name__} offsets cannot be empty")
        self.rrule_kwargs = {}
        periods = []
        rrule_periods = []
        non_rrule_periods = []
        for offset in self._iter_offsets():
            periods.append(offset.period)
            if offset.rrule_kwargs:
                self.rrule_kwargs |= offset.rrule_kwargs
                rrule_periods.append(offset.period)
            else:
                non_rrule_periods.append(offset.period)

        def by_unit(period: Period) -> Unit:
            return period.unit._n if period.unit is not None else 0  # smaller than all units
        self.min_period = min(periods, default=None, key=by_unit)
        self.rrule_period = min(rrule_periods, default=None, key=by_unit)
        self.non_rrule_period = min(non_rrule_periods, default=None, key=by_unit)
        self.unit = self.min_period.unit
        self.range = max(periods, default=None, key=by_unit).unit

    def __rsub__(self, other: datetime.datetime) -> Interval:
        if self.unit is None:
            return Interval(None, None)
        start = self.min_period.unit.truncate(other)
        if self.rrule_period is not None:
            # HACK: rrule requires a starting point even when going backwards.
            # So we use a big one, but this is inefficient
            dtstart = start - Unit.YEAR.relativedelta(100)
            while True:
                # find the start and interval using the rrule
                start = dateutil.rrule.rrule(dtstart=dtstart, **self.rrule_kwargs).before(start, inc=True)
                if start is None:
                    raise ValueError(f"no {self.rrule_kwargs} between {dtstart} and {start}")
                interval = start + self.rrule_period

                # subtract off any non-rrule period
                if self.non_rrule_period is not None:
                    interval = start - self.non_rrule_period

                    # if outside the valid range of the rrule, move back in
                    if interval.start < self.rrule_period.unit.truncate(start):
                        interval = Interval(
                            interval.start + self.rrule_period.unit.relativedelta(self.rrule_period.n),
                            interval.end + self.rrule_period.unit.relativedelta(self.rrule_period.n))

                # start is guaranteed to be before other by rrule, but end is not
                if interval.end <= other:
                    break
                start -= Unit.MICROSECOND.relativedelta(1)
        elif self.non_rrule_period is not None:
            interval = start - self.non_rrule_period
        else:
            raise ValueError(f"{self.rrule_period} and {self.non_rrule_period} are both None")
        return interval

    def __radd__(self, other: datetime.datetime) -> Interval:
        if self.unit is None:
            return Interval(None, None)
        start = self.min_period.unit.truncate(other)
        if start < other:
            start += self.min_period.unit.relativedelta(self.min_period.n)
        if self.rrule_period is not None:
            start = dateutil.rrule.rrule(dtstart=start, **self.rrule_kwargs).after(start, inc=True)
            if start is None:
                raise ValueError(f"no {self.rrule_kwargs} between {start} and {other}")
        return start + self.min_period


@_dataclass
class Year(Interval):
    digits: int
    n_missing_digits: int = 0
    start: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    end: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        duration_in_years = 10 ** self.n_missing_digits
        self.start = datetime.datetime(year=self.digits * duration_in_years, month=1, day=1)
        self.end = self.start + dateutil.relativedelta.relativedelta(years=duration_in_years)


@_dataclass
class YearSuffix(Interval):
    interval: Interval
    last_digits: int
    n_suffix_digits: int
    n_missing_digits: int = 0
    start: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    end: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        divider = 10 ** (self.n_suffix_digits + self.n_missing_digits)
        multiplier = 10 ** self.n_suffix_digits
        digits = self.interval.start.year // divider * multiplier + self.last_digits
        self.start, self.end = Year(digits, self.n_missing_digits)


@_dataclass
class IntervalOp(Interval):
    interval: Interval
    offset: Offset
    start: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    end: datetime.datetime | None = dataclasses.field(init=False, repr=False)


@_dataclass
class Last(IntervalOp):
    interval_included: bool = False
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if not self.interval.is_defined():
            self.start = None
            self.end = None
        elif self.offset is None:
            self.start = None
            self.end = self.interval.start
        else:
            start = self.interval.end if self.interval_included else self.interval.start
            self.start, self.end = start - self.offset


@_dataclass
class Next(IntervalOp):
    interval_included: bool = False
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if not self.interval.is_defined():
            self.start = None
            self.end = None
        elif self.offset is None:
            self.start = self.interval.end
            self.end = None
        else:
            if self.interval_included:
                end = self.interval.start
                # to allow repeating intervals to start with our start, subtract a tiny amount
                if isinstance(self.offset, (Repeating, OffsetUnion, RepeatingIntersection)):
                    end -= Unit.MICROSECOND.relativedelta(1)
            else:
                end = self.interval.end
            self.start, self.end = end + self.offset


@_dataclass
class Before(IntervalOp):
    n: int = 1
    interval_included: bool = False
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if not self.interval.is_defined():
            self.start = None
            self.end = None
        elif isinstance(self.offset, (Repeating, OffsetUnion, RepeatingIntersection)):
            start = self.interval.end if self.interval_included else self.interval.start
            for i in range(self.n - 1):
                start = (start - self.offset).start
            self.start, self.end = start - self.offset
        elif isinstance(self.offset, (Period, PeriodSum)):
            if self.interval_included:
                raise ValueError("interval_included=True cannot be used with Periods")
            self.start, self.end = self.interval
            for i in range(self.n):
                self.start = (self.start - self.offset).start
                self.end = (self.end - self.offset).start
        elif self.offset is None:
            self.start = None
            self.end = self.interval.start
        else:
            raise NotImplementedError


@_dataclass
class After(IntervalOp):
    n: int = 1
    interval_included: bool = False
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if not self.interval.is_defined():
            self.start = None
            self.end = None
        elif isinstance(self.offset, (Repeating, OffsetUnion, RepeatingIntersection)):
            # to allow repeating intervals to overlap start with our start, subtract a tiny amount
            end = self.interval.start - Unit.MICROSECOND.relativedelta(
                1) if self.interval_included else self.interval.end
            for i in range(self.n - 1):
                end = (end + self.offset).end
            self.start, self.end = end + self.offset
        elif isinstance(self.offset, (Period, PeriodSum)):
            if self.interval_included:
                raise ValueError("interval_included=True cannot be used with Periods")
            self.start, self.end = self.interval
            for i in range(self.n):
                self.start = (self.start + self.offset).end
                self.end = (self.end + self.offset).end
        elif self.offset is None:
            self.start = self.interval.end
            self.end = None
        else:
            raise NotImplementedError


@_dataclass
class Nth(IntervalOp):
    index: int
    from_end: bool = False
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if self.offset is None or (self.from_end and self.interval.end is None) \
                or (not self.from_end and self.interval.start is None):
            self.start = None
            self.end = None
        else:
            offset = self.interval.end if self.from_end else self.interval.start
            # to allow repeating intervals to overlap start with our start, subtract a tiny amount
            if isinstance(self.offset, (Repeating, OffsetUnion, RepeatingIntersection)) \
                    and not self.from_end and not offset == datetime.datetime.min:
                offset -= Unit.MICROSECOND.relativedelta(1)
            for i in range(self.index - 1):
                offset = (offset - self.offset).start if self.from_end else (offset + self.offset).end
            self.start, self.end = offset - self.offset if self.from_end else offset + self.offset
            if (self.start is not None and self.interval.start is not None and self.start < self.interval.start) or \
                    (self.end is not None and self.interval.end is not None and self.end > self.interval.end):
                raise ValueError(f"{self.isoformat()} is not within {self.interval.isoformat()}:\n{self}")


@_dataclass
class This(IntervalOp):
    interval: Interval
    offset: Offset
    start: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    end: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if not self.interval.is_defined() or self.offset is None:
            self.start = None
            self.end = None
        elif isinstance(self.offset, (Repeating, OffsetUnion, RepeatingIntersection)):
            if self.offset.range is None:
                self.start = self.end = None
            else:
                start = self.offset.range.truncate(self.interval.start)
                self.start, self.end = start - Unit.MICROSECOND.relativedelta(1) + self.offset
                if (self.end + self.offset).end < self.interval.end:
                    raise ValueError(f"there is more than one {self.offset} in {self.interval.isoformat()}")
        elif isinstance(self.offset, (Period, PeriodSum)):
            self.start, self.end = self.offset.expand(self.interval)
        else:
            raise NotImplementedError


@_dataclass
class Between(Interval):
    start_interval: Interval
    end_interval: Interval
    start_included: bool = False
    end_included: bool = False
    start: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    end: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if not self.start_interval.is_defined() or not self.end_interval.is_defined():
            self.start = None
            self.end = None
        else:
            self.start = self.start_interval.start if self.start_included else self.start_interval.end
            self.end = self.end_interval.end if self.end_included else self.end_interval.start
            if self.end < self.start:
                start_iso = self.start_interval.isoformat()
                end_iso = self.end_interval.isoformat()
                raise ValueError(f"{start_iso} is not before {end_iso}:\n{self}")


@_dataclass
class Intersection(Interval):
    intervals: typing.Iterable[Interval]
    start: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    end: datetime.datetime | None = dataclasses.field(init=False, repr=False)
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if any(i.start is None and i.end is None for i in self.intervals):
            self.start = self.end = None
        else:
            self.start = max((i.start for i in self.intervals if i.start is not None), default=None)
            self.end = min((i.end for i in self.intervals if i.end is not None), default=None)
        if self.start is not None and self.end is not None and self.start >= self.end:
            raise ValueError(f"{self.start.isoformat()} is not before {self.end.isoformat()}")


class Intervals(collections.abc.Iterable[Interval], abc.ABC):
    def isoformats(self) -> collections.abc.Iterator[str]:
        for interval in self:
            yield interval.isoformat()


@_dataclass
class _N(Intervals):
    interval: Interval
    offset: Offset
    n: int
    interval_included: bool = False
    base_class: type = None

    def _adjust_for_n_none(self, interval: Interval):
        raise NotImplementedError

    def __iter__(self) -> typing.Iterator[Interval]:
        interval = self.interval
        interval_included = self.interval_included
        n = 2 if self.n is None else self.n
        for i in range(n):
            interval = self.base_class(interval, self.offset, interval_included)
            if self.n is None and i == 1:
                self._adjust_for_n_none(interval)
            yield interval
            if i == 0:
                interval_included = False


@_dataclass
class LastN(_N):
    base_class: type = Last
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def _adjust_for_n_none(self, interval: Interval):
        interval.start = None


@_dataclass
class NextN(_N):
    base_class: type = Next
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def _adjust_for_n_none(self, interval: Interval):
        interval.end = None


@_dataclass
class NthN(Intervals):
    interval: Interval
    offset: Offset
    index: int
    n: int
    from_end: bool = False
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __iter__(self) -> typing.Iterator[Interval]:
        n = 2 if self.n is None else self.n
        start = 1 + (self.index - 1) * n
        for index in range(start, start + n):
            interval = Nth(self.interval, self.offset, index, from_end=self.from_end)
            if self.n is None and index == start + 1:
                if self.from_end:
                    interval.start = None
                else:
                    interval.end = None
            yield interval


@_dataclass
class These(Intervals):
    interval: Interval
    offset: Offset
    span: (int, int) = dataclasses.field(default=None, repr=False)

    def __post_init__(self):
        if not self.interval.is_defined() or self.offset is None:
            start = None
            end = None
        else:
            if isinstance(self.offset, Repeating):
                range_unit = self.offset.range
            else:
                range_unit = self.offset.unit
            start = range_unit.truncate(self.interval.start)
            end = range_unit.truncate(self.interval.end)
            if end != self.interval.end:
                _, end = end + Repeating(range_unit)
        self.interval = Interval(start, end)

    def __iter__(self) -> typing.Iterator[Interval]:
        # without a start, we can't find anything
        if self.interval.start is None:
            yield Interval(None, None)
        # without an end, we would generate an infinite number of intervals
        elif self.interval.end is None:
            yield Interval(None, None)
        else:
            interval = self.interval.start + self.offset
            while True:
                if interval.end is None:
                    yield Interval(None, None)
                    break
                if interval.end > self.interval.end:
                    break
                yield interval
                interval = interval.end + self.offset


def from_xml(elem: ET.Element, known_intervals: dict[(int, int), Interval] = None):
    if known_intervals is None:
        known_intervals = {}

    @_dataclass
    class Number:
        value: int | float
        offset: Offset = None
        span: (int, int) = dataclasses.field(default=None, repr=False)

    @_dataclass
    class AMPM:
        value: str
        span: (int, int) = dataclasses.field(default=None, repr=False)

    id_to_entity = {}
    id_to_children = {}
    id_to_n_parents = collections.Counter()
    for entity in elem.findall(".//entity"):
        entity_id = entity.findtext("id")
        if entity_id in id_to_entity:
            other = id_to_entity[entity_id]
            raise ValueError(f"duplicate id {entity_id} on {ET.tostring(entity)} and {ET.tostring(other)}")
        id_to_entity[entity_id] = entity
        id_to_children[entity_id] = set()
        for prop in entity.find("properties"):
            if prop.text and '@' in prop.text:
                id_to_children[entity_id].add(prop.text)
                id_to_n_parents[prop.text] += 1

    # to avoid infinite loops below, remove non-existent entities (i.e., values that are not keys)
    for key in id_to_children:
        id_to_children[key].intersection_update(id_to_children.keys())

    # topological sort
    sorted_ids = {}
    while id_to_children:
        for key in list(id_to_children):
            if not id_to_children[key]:
                id_to_children.pop(key)
                sorted_ids[key] = True
        for key, values in id_to_children.items():
            id_to_children[key] -= sorted_ids.keys()

    id_to_obj = {}
    for entity_id in sorted_ids:
        entity = id_to_entity[entity_id]
        sub_interval_id = entity.findtext("properties/Sub-Interval")
        super_interval_id = entity.findtext("properties/Super-Interval")
        entity_type = entity.findtext("type")
        prop_value = entity.findtext("properties/Value")
        prop_type = entity.findtext("properties/Type")
        prop_number = entity.findtext("properties/Number")
        spans = []

        # TODO: revisit whether discontinuous spans need to be retained
        char_offsets = {int(x)
                        for start_end in entity.findtext("span").split(";")
                        for x in start_end.split(",")}
        trigger_span = (min(char_offsets), max(char_offsets))

        # helper for managing access to id_to_obj
        def pop(obj_id: str) -> Interval | Offset | Period | Repeating | Number | AMPM:
            result = id_to_obj[obj_id]
            id_to_n_parents[obj_id] -= 1
            if not id_to_n_parents[obj_id]:
                id_to_obj.pop(obj_id)
            if result.__class__ is not Interval:  # raw Interval has no span attribute
                spans.append(result.span)
            return result

        # helper for ET.findall + text + pop
        def pop_all_prop(prop_name: str) -> list[Interval | Offset | Period | Repeating | Number | AMPM]:
            return [pop(e.text) for e in entity.findall(f"properties/{prop_name}") if e.text]

        # helper for managing the multiple interval properties
        def get_interval(prop_name: str) -> Interval:
            prop_interval_type = entity.findtext(f"properties/{prop_name}-Type")
            prop_interval = entity.findtext(f"properties/{prop_name}")
            match prop_interval_type:
                case "Link":
                    return pop(prop_interval)
                case "DocTime" if (None, None) in known_intervals:
                    return known_intervals.get((None, None))
                case "DocTime-Year" if (None, None) in known_intervals:
                    doc_time = known_intervals.get((None, None))
                    return Year(doc_time.start.year)
                case "DocTime" | "DocTime-Year":
                    raise ValueError(f"known_intervals[(None, None)] required")
                case "DocTime-Era":
                    return Interval(datetime.datetime.min, None)
                case "Unknown":
                    return Interval(None, None)
                case other_type:
                    raise NotImplementedError(other_type)

        # helper for managing the multiple offset properties
        def get_offset() -> Offset:
            prop_offset = entity.findtext("properties/Period") or entity.findtext("properties/Repeating-Interval")
            return pop(prop_offset) if prop_offset else None

        # helper for managing Included properties
        def get_included(prop_name: str) -> bool:
            match entity.findtext(f"properties/{prop_name}"):
                case "Included" | "Interval-Included":
                    return True
                case "Not-Included" | "Interval-Not-Included" | "Standard":
                    return False
                case other_type:
                    raise NotImplementedError(other_type)

        # create objects from <entity> elements
        try:
            match entity_type:
                case "Period":
                    if prop_type == "Unknown":
                        unit = None
                    else:
                        unit_name = prop_type.upper()
                        unit_name = re.sub(r"IES$", r"Y", unit_name)
                        unit_name = re.sub(r"S$", r"", unit_name)
                        unit_name = re.sub("-", "_", unit_name)
                        unit = Unit.__members__[unit_name]
                    if prop_number:
                        n = pop(prop_number).value
                    else:
                        n = None
                    obj = Period(unit, n)
                case "Sum":
                    obj = PeriodSum(pop_all_prop("Periods"))
                case "Year" | "Two-Digit-Year":
                    digits_str = prop_value.rstrip('?')
                    n_missing_digits = len(prop_value) - len(digits_str)
                    digits = int(digits_str)
                    match entity_type:
                        case "Year":
                            obj = Year(digits, n_missing_digits)
                        case "Two-Digit-Year":
                            n_suffix_digits = 2 - n_missing_digits
                            obj = YearSuffix(get_interval("Interval"), digits,
                                             n_suffix_digits, n_missing_digits)
                        case other:
                            raise NotImplementedError(other)
                case "Month-Of-Year":
                    month_int = datetime.datetime.strptime(prop_type, '%B').month
                    obj = Repeating(Unit.MONTH, Unit.YEAR, value=month_int)
                case "Day-Of-Month":
                    obj = Repeating(Unit.DAY, Unit.MONTH, value=int(prop_value))
                case "Day-Of-Week":
                    day_int = getattr(dateutil.relativedelta, prop_type.upper()[:2]).weekday
                    obj = Repeating(Unit.DAY, Unit.WEEK, value=day_int)
                case "AMPM-Of-Day":
                    obj = AMPM(prop_type)
                case "Hour-Of-Day":
                    hour = int(prop_value)
                    prop_am_pm = entity.findtext("properties/AMPM-Of-Day")
                    if prop_am_pm:
                        match pop(prop_am_pm).value:
                            case "AM" if hour == 12:
                                hour = 0
                            case "PM" if hour != 12:
                                hour += 12
                            case "AM" | "PM":
                                pass
                            case other:
                                raise NotImplementedError(other)
                    obj = Repeating(Unit.HOUR, Unit.DAY, value=hour)
                case "Minute-Of-Hour":
                    obj = Repeating(Unit.MINUTE, Unit.HOUR, value=int(prop_value))
                case "Second-Of-Minute":
                    obj = Repeating(Unit.SECOND, Unit.MINUTE, value=int(prop_value))
                case "Part-Of-Day" | "Season-Of-Year" if prop_type in {"Unknown", "Dawn", "Dusk"}:
                    # TODO: improve handling of location-dependent times
                    obj = Repeating(None)
                case "Part-Of-Day" | "Part-Of-Week" | "Season-Of-Year":
                    obj = globals()[prop_type]()
                case "Calendar-Interval":
                    unit_name = prop_type.upper().replace("-", "_")
                    obj = Repeating(Unit.__members__[unit_name])
                case "Union":
                    obj = OffsetUnion(pop_all_prop("Repeating-Intervals"))
                case "Every-Nth":
                    obj = EveryNth(get_offset(), int(prop_value))
                case "Last" | "Next" | "Before" | "After" | "NthFromEnd" | "NthFromStart":
                    cls_name = "Nth" if entity_type.startswith("Nth") else entity_type
                    interval = get_interval("Interval")
                    offset = get_offset()
                    kwargs = {}
                    match cls_name:
                        case "Last" | "Next" | "Before" | "After":
                            kwargs["interval_included"] = get_included("Semantics")
                        case "Nth":
                            kwargs["index"] = int(prop_value)
                            kwargs["from_end"] = entity_type == "NthFromEnd"
                    if isinstance(offset, Number):
                        kwargs["n"] = offset.value
                        if cls_name not in {"Before", "After"}:
                            cls_name += "N"
                        offset = offset.offset
                    obj = globals()[cls_name](interval=interval, offset=offset, **kwargs)
                case "This":
                    obj = This(get_interval("Interval"), get_offset())
                case "Between":
                    obj = Between(get_interval("Start-Interval"),
                                  get_interval("End-Interval"),
                                  start_included=get_included("Start-Included"),
                                  end_included=get_included("End-Included"))
                case "Intersection":
                    match (pop_all_prop("Intervals"), pop_all_prop("Repeating-Intervals")):
                        case intervals, []:
                            obj = Intersection(intervals)
                        case [], repeating_intervals:
                            obj = RepeatingIntersection(repeating_intervals)
                        case [interval], [repeating_interval]:
                            obj = This(interval, repeating_interval)
                        case [interval], repeating_intervals:
                            obj = This(interval, RepeatingIntersection(repeating_intervals))
                        case other:
                            raise NotImplementedError(other)
                case "Number":
                    if prop_value == '?':
                        value = None
                    elif prop_value.isdigit():
                        value = int(prop_value)
                    else:
                        try:
                            value = float(prop_value)
                        except ValueError:
                            # TODO: handle ranges better
                            value = None
                    obj = Number(value)
                case "Event":
                    obj = known_intervals.get(trigger_span)
                    if obj is None:
                        obj = Interval(None, None)
                case "Time-Zone" | "Modifier" | "Frequency" | "NotNormalizable" | "PreAnnotation":
                    # TODO: handle time zones, modifiers, and frequencies
                    continue
                case other:
                    raise NotImplementedError(other)

            # add spans to objects
            obj.span = obj.trigger_span = trigger_span
            spans.append(obj.span)

            # if Number property is present, wrap offset with number for later use
            # skip this for Periods, which directly consume their Number above
            if prop_number and not isinstance(obj, Period):
                repeating_n = pop(prop_number)
                repeating_n.offset = obj
                obj = repeating_n

            # create additional objects as necessary for sub-intervals
            if sub_interval_id:
                sub_interval = pop(sub_interval_id)
                match entity_type:
                    case "Year" | "Two-Digit-Year":
                        obj = This(obj, sub_interval)
                    case "Month-Of-Year" | "Day-Of-Month" | "Day-Of-Week" | \
                         "Part-Of-Week" | "Part-Of-Day" | \
                         "Hour-Of-Day" | "Minute-Of-Hour" | "Second-Of-Minute":
                        obj = RepeatingIntersection([obj, sub_interval])
                    case other:
                        raise NotImplementedError(other)

            # create additional objects as necessary for super-intervals
            if super_interval_id:
                super_interval = pop(super_interval_id)
                match super_interval:
                    case Year() | YearSuffix() | This():
                        obj = This(super_interval, obj)
                    case Repeating():
                        obj = RepeatingIntersection([super_interval, obj])
                    case other:
                        raise NotImplementedError(other)

            obj.span = (min(start for start, _ in spans), max(end for _, end in spans))

        except Exception as e:
            raise AnaforaXMLParsingError(entity, trigger_span) from e

        # add the object to the mapping
        id_to_obj[entity_id] = obj

    # remove any Number objects as they're internal implementation details
    for key in list(id_to_obj):
        if isinstance(id_to_obj[key], Number):
            del id_to_obj[key]

    return list(id_to_obj.values())


class AnaforaXMLParsingError(RuntimeError):
    def __init__(self, entity: ET.Element, trigger_span: (int, int)):
        self.entity = entity
        self.trigger_span = trigger_span
        super().__init__(re.sub(r"\s+", "", ET.tostring(entity, encoding="unicode")))


def flatten(offset_or_interval: Offset | Interval) -> Offset | Interval:
    match offset_or_interval:
        case IntervalOp() as io:
            return dataclasses.replace(io, offset=flatten(io.offset))
        case RepeatingIntersection() as ri if any(isinstance(o, RepeatingIntersection) for o in ri.offsets):
            offsets = []
            for offset in ri.offsets:
                offset = flatten(offset)
                if isinstance(offset, RepeatingIntersection):
                    offsets.extend(offset.offsets)
                else:
                    offsets.append(offset)
            return dataclasses.replace(ri, offsets=offsets)
        case _:
            return offset_or_interval


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("xml_dir")
    parser.add_argument("--xml-suffix", default=".TimeNorm.gold.completed.xml")
    parser.add_argument("--text-dir")
    parser.add_argument("--dct-dir")
    parser.add_argument("--silent", action="store_true")
    parser.add_argument("--flatten", action="store_true")
    args = parser.parse_args()

    # iterate over the selected Anafora XML paths
    xml_paths = list(pathlib.Path(args.xml_dir).glob(f"**/*{args.xml_suffix}"))
    if not xml_paths:
        parser.exit(message=f"no such paths: {args.xml_dir}/**/*.{args.xml_suffix}\n")
    for xml_path in xml_paths:

        # load the document creation time, if provided
        if args.dct_dir is not None:
            dct_name = xml_path.name.replace(args.xml_suffix, ".dct")
            dct_path = pathlib.Path(args.dct_dir) / dct_name
            with open(dct_path) as dct_file:
                [year_str, month_str, day_str] = dct_file.read().strip().split("-")
                doc_time = Interval.of(int(year_str), int(month_str), int(day_str))

        # use today for the document creation time, if not provided
        else:
            today = datetime.date.today()
            doc_time = Interval.of(today.year, today.month, today.day)

        # parse the Anafora XML into Intervals, Offsets, etc.
        elem = ET.parse(xml_path).getroot()
        try:
            for obj in from_xml(elem, known_intervals={(None, None): doc_time}):
                if args.flatten:
                    obj = flatten(obj)
                if not args.silent:
                    print(obj)
        except AnaforaXMLParsingError as e:
            text_name = xml_path.name.replace(args.xml_suffix, "")
            text_dir = pathlib.Path(args.text_dir) if args.text_dir else xml_path.parent
            with open(text_dir / text_name) as text_file:
                text = text_file.read()

            start, end = e.trigger_span
            pre_text = text[max(0, start - 100):start]
            post_text = text[end:min(len(text), end + 100)]
            traceback.print_exception(e.__cause__)
            msg = f"\nContext:\n{pre_text}[[{text[start:end]}]]{post_text}\nXML:\n{e}\nFile:\n{xml_path}\n"
            print(msg, file=sys.stderr)
