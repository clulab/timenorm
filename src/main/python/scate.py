import dataclasses
import datetime
import dateutil.relativedelta
import dateutil.rrule
import enum


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


class Field(enum.Enum):
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
class RepeatingUnit(Offset):
    unit: Unit

    def __post_init__(self):
        self._one_unit = self.unit.relativedelta(1)

    def __rsub__(self, other: datetime.datetime) -> Interval:
        end = self.unit.truncate(other)
        return Interval(end - self._one_unit, end)
    
    def __radd__(self, other: datetime.datetime) -> Interval:
        start = self.unit.truncate(other)
        if start < other:
            start += self._one_unit
        return Interval(start, start + self._one_unit)


@dataclasses.dataclass
class RepeatingField(Offset):
    field: Field
    value: int

    def __post_init__(self):
        self.unit = self.field.base
        self._rrule_kwargs = self.field.range.rrule_kwargs() | self.field.rrule_kwargs(self.value)
        self._one_range = self.field.range.relativedelta(1)
        self._one_base = self.field.base.relativedelta(1)

    def __rsub__(self, other: datetime.datetime) -> Interval:
        # rrule requires a starting point even when going backwards,
        # so start at twice the expected range
        dtstart = other - 2 * self._one_range
        ldt = dateutil.rrule.rrule(dtstart=dtstart, **self._rrule_kwargs).before(other)
        ldt = self.field.base.truncate(ldt)
        return Interval(ldt, ldt + self._one_base)
    
    def __radd__(self, other: datetime.datetime) -> Interval:
        ldt = dateutil.rrule.rrule(dtstart=other, **self._rrule_kwargs).after(other)
        ldt = self.field.base.truncate(ldt)
        return Interval(ldt, ldt + self._one_base)


@dataclasses.dataclass
class RRuleOffset(Offset):
    unit: Unit
    rrule_kwargs: dict
    start_rrule_kwargs: dict
    end_rrule_kwargs: dict

    def __post_init__(self):
        self._one_unit = self.unit.relativedelta(1)
        self._one_micro = Unit.MICROSECOND.relativedelta(1)
        self.start_rrule_kwargs.update(self.rrule_kwargs)
        self.end_rrule_kwargs.update(self.rrule_kwargs)

    def __rsub__(self, other: datetime.datetime) -> Interval:
        # rrule requires a starting point even when going backwards,
        # so start at twice the expected range
        dtstart = other - 2 * self._one_unit
        end = dateutil.rrule.rrule(dtstart=dtstart, **self.end_rrule_kwargs).before(other)
        start = dateutil.rrule.rrule(dtstart=dtstart, **self.start_rrule_kwargs).before(end)
        return Interval(start, end)

    def __radd__(self, other: datetime.datetime) -> Interval:
        # we need to allow start == other, so move other back the smallest amount possible
        other -= self._one_micro
        start = dateutil.rrule.rrule(dtstart=other, **self.start_rrule_kwargs).after(other)
        end = dateutil.rrule.rrule(dtstart=start, **self.end_rrule_kwargs).after(start)
        return Interval(start, end)


class Season:
    # Defined as "meterological seasons"
    # https://www.ncei.noaa.gov/news/meteorological-versus-astronomical-seasons
    SPRING = RRuleOffset(
        unit=Unit.YEAR,
        rrule_kwargs=dict(bymonthday=1, byhour=0, byminute=0, bysecond=0, freq=dateutil.rrule.DAILY),
        start_rrule_kwargs=dict(bymonth=3),
        end_rrule_kwargs=dict(bymonth=6))

    SUMMER = RRuleOffset(
        unit=Unit.YEAR,
        rrule_kwargs=dict(bymonthday=1, byhour=0, byminute=0, bysecond=0, freq=dateutil.rrule.DAILY),
        start_rrule_kwargs=dict(bymonth=6),
        end_rrule_kwargs=dict(bymonth=9))

    FALL = AUTUMN = RRuleOffset(
        unit=Unit.YEAR,
        rrule_kwargs=dict(bymonthday=1, byhour=0, byminute=0, bysecond=0, freq=dateutil.rrule.DAILY),
        start_rrule_kwargs=dict(bymonth=9),
        end_rrule_kwargs=dict(bymonth=12))

    WINTER = RRuleOffset(
        unit=Unit.YEAR,
        rrule_kwargs=dict(bymonthday=1, byhour=0, byminute=0, bysecond=0, freq=dateutil.rrule.DAILY),
        start_rrule_kwargs=dict(bymonth=12),
        end_rrule_kwargs=dict(bymonth=3))


class DayPart:
    # defined as used in forecasts
    # https://www.weather.gov/bgm/forecast_terms
    MORNING = RRuleOffset(
        unit=Unit.DAY,
        rrule_kwargs=dict(byminute=0, bysecond=0, freq=dateutil.rrule.HOURLY),
        start_rrule_kwargs=dict(byhour=6),
        end_rrule_kwargs=dict(byhour=12))

    AFTERNOON = RRuleOffset(
        unit=Unit.DAY,
        rrule_kwargs=dict(byminute=0, bysecond=0, freq=dateutil.rrule.HOURLY),
        start_rrule_kwargs=dict(byhour=12),
        end_rrule_kwargs=dict(byhour=18))

    EVENING = RRuleOffset(
        unit=Unit.DAY,
        rrule_kwargs=dict(byminute=0, bysecond=0, freq=dateutil.rrule.HOURLY),
        start_rrule_kwargs=dict(byhour=18),
        end_rrule_kwargs=dict(byhour=0))

    NIGHT = RRuleOffset(
        unit=Unit.DAY,
        rrule_kwargs=dict(byminute=0, bysecond=0, freq=dateutil.rrule.HOURLY),
        start_rrule_kwargs=dict(byhour=0),
        end_rrule_kwargs=dict(byhour=6))


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
        if isinstance(self.offset, (RepeatingUnit, RepeatingField)):
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
        if isinstance(self.offset, (RepeatingUnit, RepeatingField)):
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
        if isinstance(self.offset, (RepeatingUnit, RepeatingField, RRuleOffset)):
            start = self.offset.unit.truncate(self.interval.start)
            self.start, self.end = start + self.offset
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
class N:
    interval_op_class: type
    interval: Interval
    offset: Offset
    n: int
    kwargs: dict = dataclasses.field(default_factory=dict)

    def __iter__(self):
        kwargs = dict(self.kwargs)
        kwargs["interval"] = self.interval
        kwargs["offset"] = self.offset

        # Last and Next are applied N times
        if self.interval_op_class in {Last, Next}:
            for i in range(self.n):
                interval = self.interval_op_class(**kwargs)
                yield interval
                kwargs["interval"] = interval
                if i == 0 and "interval_included" in kwargs:
                    kwargs.pop("interval_included")

        # Nth is applied with its index increasing N times
        elif self.interval_op_class is Nth:
            index = kwargs["index"]
            for index in range(index, index + self.n):
                kwargs["index"] = index
                yield self.interval_op_class(**kwargs)

        else:
            raise NotImplementedError
