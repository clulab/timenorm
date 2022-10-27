from enum import Enum
import datetime
from datetime import timedelta
from dateutil.relativedelta import relativedelta

class TimeUnit(Enum):
    MICROSECOND = timedelta(microseconds = 1)
    MILLISECOND = timedelta(milliseconds = 1)
    SECOND = timedelta(seconds = 1)
    MINUTE = timedelta(minutes = 1)
    HOUR = timedelta(hours = 1)
    DAY = timedelta(days = 1)
    WEEK = timedelta(days = 7)
    YEAR = timedelta(days = 365)
    DECADE = YEAR * 10
    QUARTER_CENTURY = YEAR * 25
    CENTURY = YEAR * 100
    
    @staticmethod
    def truncate(ldt: datetime.datetime, unit) -> datetime.datetime:
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
        elif unit == TimeUnit.MONTH:
            return datetime.datetime(ldt.year, ldt.month, 1, 0, 0)
        elif unit == TimeUnit.DAY:
            return datetime.datetime(ldt.year, ldt.month, ldt.day, 0, 0)
        elif unit == TimeUnit.HOUR:
            return datetime.datetime(ldt.year, ldt.month, ldt.day, ldt.hour, 0)
        elif unit == TimeUnit.MINUTE:
            return datetime.datetime(ldt.year, ldt.month, ldt.day, ldt.hour, ldt.minute)
        elif unit == TimeUnit.SECOND:
            return datetime.datetime(ldt.year, ldt.month, ldt.day, ldt.hour, ldt.minute, ldt.second)
        elif unit == TimeUnit.MILLISECOND:
            return datetime.datetime(ldt.year, ldt.month, ldt.day, ldt.hour, ldt.minute, ldt.second, ldt.millisecond)
        else:
            return ldt
    
    def map_to_datetime(self):
        return {a.name: a.name.lower() + 's' for a in filter(lambda a: not a.startswith('__'), dir(self))}
        
        
        