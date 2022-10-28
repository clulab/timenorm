from enum import Enum
import datetime
from datetime import timedelta
from dateutil.relativedelta import relativedelta

class TimeUnit(Enum):
    # values of units are ISO format string split positions, for truncation
    # names with values geq -999 are special cases - TODO: this seems clunky? maybe shouldn't use Enum?
    MICROSECOND = -999 
    MILLISECOND = 23
    SECOND = 19
    MINUTE = 16
    HOUR = 13
    DAY = 10
    WEEK = -1000
    MONTH = -1001
    YEAR = -1002
    QUARTER_YEAR = -1003
    DECADE = -1004
    QUARTER_CENTURY = -1005
    CENTURY = -1006
    
    @staticmethod
    def truncate(ldt: datetime.datetime, unit) -> datetime.datetime:
        if unit == TimeUnit.MICROSECOND:
            return ldt
        elif not (unit.value <= -999): # if we can use iso string to truncate
            return datetime.datetime.fromisoformat(ldt.isoformat()[:unit.value])
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
            elif unit == TimeUnit.MONTH:
                return datetime.datetime(ldt.year, ldt.month, 1, 0, 0)
            elif unit == TimeUnit.WEEK:
                week_start = datetime.date.fromordinal(ldt.timetuple().tm_yday - ldt.timetuple().tm_wday)
                return datetime.datetime(ldt.year, week_start.month, week_start.day, 0, 0)
    
    @property
    def dt_format(self):
        if self.name not in ["YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "SECOND", "MICROSECOND"]:
            raise AttributeError(f"{self.name} unit type does not exist for datetime objects")
        return self.name.lower()
    
    @property
    def rd_format(self):
        if self.name not in ["YEAR", "MONTH", "WEEK", "DAY", "HOUR", "MINUTE", "SECOND", "MICROSECOND"]:
            raise AttributeError(f"{self.name} unit type does not exist in dateutil.relativedelta")
        return self.dt_format + "s"
        
        
        