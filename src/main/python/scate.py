import collections.abc
import dataclasses
import datetime
import dateutil.relativedelta
import dateutil.rrule
import enum
import typing


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
    MICROSECOND = (1, "microseconds", None)
    MILLISECOND = (2, None, None)
    SECOND = (3, "seconds", dateutil.rrule.SECONDLY)
    MINUTE = (4, "minutes", dateutil.rrule.MINUTELY)
    HOUR = (5, "hours", dateutil.rrule.HOURLY)
    DAY = (6, "days", dateutil.rrule.DAILY)
    WEEK = (7, "weeks", dateutil.rrule.WEEKLY)
    MONTH = (8, "months", dateutil.rrule.MONTHLY)
    QUARTER_YEAR = (9, None, None)
    YEAR = (10, "years", dateutil.rrule.YEARLY)
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
            dt = datetime.datetime(dt.year // 100 * 100, 1, 1, 0, 0)
        return dt

    def relativedelta(self, n):
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


class Offset:
    unit: Unit

    def __rsub__(self, other: datetime.datetime) -> Interval:
        raise NotImplementedError

    def __radd__(self, other: datetime.datetime) -> Interval:
        raise NotImplementedError


@dataclasses.dataclass
class Period(Offset):
    unit: Unit
    n: int

    def __radd__(self, other: datetime.datetime) -> Interval:
        return Interval(other, other + self.unit.relativedelta(self.n))

    def __rsub__(self, other: datetime.datetime) -> Interval:
        return Interval(other - self.unit.relativedelta(self.n), other)

    def expand(self, interval: Interval) -> Interval:
        return self.unit.expand(interval, self.n)


@dataclasses.dataclass
class Sum(Offset):
    periods: list[Period]

    def __post_init__(self):
        self.unit = max(self.periods, key=lambda p: p.unit._n).unit

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


@dataclasses.dataclass
class Repeating(Offset):
    unit: Unit
    range: Unit = None
    value: int = dataclasses.field(default=None, kw_only=True)
    n_units: int = dataclasses.field(default=1, kw_only=True)
    rrule_kwargs: dict = dataclasses.field(default_factory=dict, kw_only=True)

    def __post_init__(self):
        self.period = Period(self.unit, self.n_units)
        if self.range is None:
            self.range = self.unit
        elif self.value is None:
            raise ValueError(f"value=None is not allowed for range={self.range}")
        else:
            self.rrule_kwargs |= self.range.rrule_kwargs()
            match (self.unit, self.range):
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
        start = self.unit.truncate(other)
        if self.rrule_kwargs:
            start = dateutil.rrule.rrule(dtstart=start, **self.rrule_kwargs).after(other)
        elif start < other:
            start += self.period.unit.relativedelta(self.period.n)
        return start + self.period


class Season:
    # Defined as "meterological seasons"
    # https://www.ncei.noaa.gov/news/meteorological-versus-astronomical-seasons
    SPRING = Repeating(
        unit=Unit.MONTH,
        range=Unit.YEAR,
        value=3,
        n_units=3)

    SUMMER = Repeating(
        unit=Unit.MONTH,
        range=Unit.YEAR,
        value=6,
        n_units=3)

    FALL = AUTUMN = Repeating(
        unit=Unit.MONTH,
        range=Unit.YEAR,
        value=9,
        n_units=3)

    WINTER = Repeating(
        unit=Unit.MONTH,
        range=Unit.YEAR,
        value=12,
        n_units=3)


class DayPart:
    # defined as used in forecasts
    # https://www.weather.gov/bgm/forecast_terms
    MORNING = Repeating(
        unit=Unit.HOUR,
        range=Unit.DAY,
        value=6,
        n_units=6)

    AFTERNOON = Repeating(
        unit=Unit.HOUR,
        range=Unit.DAY,
        value=12,
        n_units=6)

    EVENING = Repeating(
        unit=Unit.HOUR,
        range=Unit.DAY,
        value=18,
        n_units=6)

    NIGHT = Repeating(
        unit=Unit.HOUR,
        range=Unit.DAY,
        value=0,
        n_units=6)


@dataclasses.dataclass
class Union(Offset):
    offsets: typing.Iterable[Offset]

    def __post_init__(self):
        self.unit = min((o.unit for o in self.offsets), key=lambda unit: unit._n)

    def __rsub__(self, other: datetime.datetime) -> Interval:
        return max((other - offset for offset in self.offsets),
                   key=lambda i: (i.end, i.end - i.start))

    def __radd__(self, other: datetime.datetime) -> Interval:
        return min((other + offset for offset in self.offsets),
                   key=lambda i: (i.start, i.start - i.end))


@dataclasses.dataclass
class Intersection(Offset):
    offsets: typing.Iterable[Repeating]

    def __post_init__(self):
        if not self.offsets:
            raise ValueError(f"{self.__class__.__name__} offsets cannot be empty")
        self.rrule_kwargs = {}
        periods = []
        rrule_periods = []
        non_rrule_periods = []
        for offset in self.offsets:
            periods.append(offset.period)
            if offset.rrule_kwargs:
                self.rrule_kwargs |= offset.rrule_kwargs
                rrule_periods.append(offset.period)
            else:
                non_rrule_periods.append(offset.period)
        self.min_period = min(periods, default=None, key=lambda p: p.unit._n)
        self.rrule_period = min(rrule_periods, default=None, key=lambda p: p.unit._n)
        self.non_rrule_period = min(non_rrule_periods, default=None, key=lambda p: p.unit._n)

    def __rsub__(self, other: datetime.datetime) -> Interval:
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
        start = self.min_period.unit.truncate(other)
        if start < other:
            start += self.min_period.unit.relativedelta(self.min_period.n)
        if self.rrule_period is not None:
            start = dateutil.rrule.rrule(dtstart=start, **self.rrule_kwargs).after(start, inc=True)
            if start is None:
                raise ValueError(f"no {self.rrule_kwargs} between {start} and {other}")
        return start + self.min_period


@dataclasses.dataclass
class Year(Interval):
    digits: int
    n_missing_digits: int = 0
    start: datetime.datetime = dataclasses.field(init=False)
    end: datetime.datetime = dataclasses.field(init=False)

    def __post_init__(self):
        duration_in_years = 10 ** self.n_missing_digits
        self.start = datetime.datetime(year=self.digits * duration_in_years, month=1, day=1)
        self.end = self.start + dateutil.relativedelta.relativedelta(years=duration_in_years)


@dataclasses.dataclass
class YearSuffix(Interval):
    interval: Interval
    last_digits: int
    n_suffix_digits: int
    n_missing_digits: int = 0
    start: datetime.datetime = dataclasses.field(init=False)
    end: datetime.datetime = dataclasses.field(init=False)

    def __post_init__(self):
        divider = 10 ** (self.n_suffix_digits + self.n_missing_digits)
        multiplier = 10 ** self.n_suffix_digits
        digits = self.interval.start.year // divider * multiplier + self.last_digits
        self.start, self.end = Year(digits, self.n_missing_digits)


@dataclasses.dataclass
class IntervalOp(Interval):
    interval: Interval
    offset: Offset
    start: datetime.datetime = dataclasses.field(init=False)
    end: datetime.datetime = dataclasses.field(init=False)


@dataclasses.dataclass
class Last(IntervalOp):
    interval_included: bool = False

    def __post_init__(self):
        start = self.interval.end if self.interval_included else self.interval.start
        self.start, self.end = start - self.offset


@dataclasses.dataclass
class Next(IntervalOp):
    interval_included: bool = False

    def __post_init__(self):
        end = self.interval.start if self.interval_included else self.interval.end
        self.start, self.end = end + self.offset


@dataclasses.dataclass
class Before(IntervalOp):
    n: int = 1
    interval_included: bool = False

    def __post_init__(self):
        if isinstance(self.offset, Repeating):
            start = self.interval.end if self.interval_included else self.interval.start
            for i in range(self.n - 1):
                start = (start - self.offset).start
            self.start, self.end = start - self.offset
        elif self.interval_included:
            raise ValueError("interval_included=True cannot be used with Periods")
        else:
            self.start, self.end = self.interval
            for i in range(self.n):
                self.start = (self.start - self.offset).start
                self.end = (self.end - self.offset).start


@dataclasses.dataclass
class After(IntervalOp):
    n: int = 1
    interval_included: bool = False

    def __post_init__(self):
        if isinstance(self.offset, Repeating):
            end = self.interval.start if self.interval_included else self.interval.end
            for i in range(self.n - 1):
                end = (end + self.offset).end
            self.start, self.end = end + self.offset
        elif self.interval_included:
            raise ValueError("interval_included=True cannot be used with Periods")
        else:
            self.start, self.end = self.interval
            for i in range(self.n):
                self.start = (self.start + self.offset).end
                self.end = (self.end + self.offset).end


@dataclasses.dataclass
class This(Interval):
    interval: Interval
    offset: Offset
    start: datetime.datetime = dataclasses.field(init=False)
    end: datetime.datetime = dataclasses.field(init=False)

    def __post_init__(self):
        if isinstance(self.offset, Repeating):
            start = self.offset.range.truncate(self.interval.start)
            self.start, self.end = start - Unit.MICROSECOND.relativedelta(1) + self.offset
            if (self.end + self.offset).end < self.interval.end:
                raise ValueError(f"there is more than one {self.offset} in {self.interval.isoformat()}")
        elif isinstance(self.offset, Period):
            self.start, self.end = self.offset.expand(self.interval)
        else:
            raise NotImplementedError


@dataclasses.dataclass
class Between(Interval):
    start_interval: Interval
    end_interval: Interval
    start_included: bool = False
    end_included: bool = False
    start: datetime.datetime = dataclasses.field(init=False)
    end: datetime.datetime = dataclasses.field(init=False)

    def __post_init__(self):
        self.start = self.start_interval.start if self.start_included else self.start_interval.end
        self.end = self.end_interval.end if self.end_included else self.end_interval.start
        if self.end < self.start:
            start_iso = self.start_interval.isoformat()
            end_iso = self.end_interval.isoformat()
            raise ValueError(f"{start_iso} is not before {end_iso}")


@dataclasses.dataclass
class Nth(IntervalOp):
    index: int
    from_end: bool = False

    def __post_init__(self):
        offset = self.interval.end if self.from_end else self.interval.start
        for i in range(self.index - 1):
            offset = (offset - self.offset).start if self.from_end else (offset + self.offset).end
        self.start, self.end = offset - self.offset if self.from_end else offset + self.offset
        if self.start < self.interval.start or self.end > self.interval.end:
            raise ValueError(f"{self.isoformat()} is not within {self.interval.isoformat()}")


@dataclasses.dataclass
class _N(collections.abc.Iterable[Interval]):
    interval: Interval
    offset: Offset
    n: int
    interval_included: bool = False
    base_class: type = None

    def __iter__(self) -> typing.Iterator[Interval]:
        interval = self.interval
        interval_included = self.interval_included
        for i in range(self.n):
            interval = self.base_class(interval, self.offset, interval_included)
            yield interval
            if i == 0:
                interval_included = False


@dataclasses.dataclass
class LastN(_N):
    base_class: type = Last


@dataclasses.dataclass
class NextN(_N):
    base_class: type = Next


@dataclasses.dataclass
class NthN(collections.abc.Iterable[Interval]):
    interval: Interval
    offset: Offset
    index: int
    n: int

    def __iter__(self) -> typing.Iterator[Interval]:
        for index in range(self.index, self.index + self.n):
            yield Nth(self.interval, self.offset, index)


@dataclasses.dataclass
class These(collections.abc.Iterable[Interval]):
    interval: Interval
    offset: Offset

    def __post_init__(self):
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
        interval = self.interval.start + self.offset
        while interval.end <= self.interval.end:
            yield interval
            interval = interval.end + self.offset
