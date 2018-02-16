A library for converting natural language expressions of dates and times
to their normalized form.

## Synchronous context free grammar-based parser

Part of the code in this library parses time expressions using
synchronous context free grammars, as described in:

> Steven Bethard. 2013.
> [A Synchronous Context Free Grammar for Time Normalization](http://www.aclweb.org/anthology/D13-1078).
> In: Proceedings of the 2013 Conference on Empirical Methods in Natural
> Language Processing, pp. 821-826.

The primary entry point for most users is `TemporalExpressionParser`,
which parses a `Temporal` from a string:

```scala
scala> import org.clulab.timenorm._, scala.util.Success
import org.clulab.timenorm._
import scala.util.Success

scala> // Create a new English parser (Italian also available)
scala> val parser = TemporalExpressionParser.en()
parser: org.clulab.timenorm.TemporalExpressionParser = org.clulab.timenorm.TemporalExpressionParser@5a654e05

scala> // parse an expression given an anchor time (here, assuming it succeeds)
scala> val Success(temporal) = parser.parse("two weeks ago", TimeSpan.of(2013, 1, 4))
temporal: org.clulab.timenorm.Temporal = TimeSpan(2012-12-17T00:00Z,2012-12-24T00:00Z,Period(Map(Weeks -> 1),Exact),Exact)

scala> // express the Temporal as a TimeML value
scala> temporal.timeMLValue
res0: String = 2012-W51
```

The library is available on Maven central as
[info.bethard.timenorm](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22info.bethard%22%20AND%20a%3A%22timenorm%22).

## Semantically compositional time operators

A newer part of the library encodes time through a set of
semantically compositional operators as described in:

> Steven Bethard and Jonathan Parker. 2016.
> [A Semantically Compositional Annotation Scheme for Time Normalization](http://www.lrec-conf.org/proceedings/lrec2016/pdf/288_Paper.pdf).
> In: Proceedings of the Tenth International Conference on Language
> Resources and Evaluation (LREC 2016). pp. 3779-3786.

The operators are available in the `org.clulab.timenorm.formal`
package, and can combine fields and units from the `java.time` library
to describe complex temporal expressions:

```scala
scala> import org.clulab.timenorm.formal._, java.time.temporal.ChronoField._, java.time.temporal.ChronoUnit._
import org.clulab.timenorm.formal._
import java.time.temporal.ChronoField._
import java.time.temporal.ChronoUnit._

scala> // the 3-year period following the year 1985
scala> NextP(Year(1985), SimplePeriod(YEARS, 3))
res0: org.clulab.timenorm.formal.NextP = NextP(Year(1985,0),SimplePeriod(Years,IntNumber(3),Exact))

scala> (res0.start, res0.end)
res1: (java.time.LocalDateTime, java.time.LocalDateTime) = (1986-01-01T00:00,1989-01-01T00:00)

scala> // the Friday the 13th following the 15th day of the 3rd month of 1985
scala> NextRI(
     |   ThisRI(
     |     ThisRI(
     |       Year(1985),
     |       RepeatingField(MONTH_OF_YEAR, 3)),
     |     RepeatingField(DAY_OF_MONTH, 15)),
     |   IntersectionRI(Set(
     |     RepeatingField(DAY_OF_WEEK, 5),
     |     RepeatingField(DAY_OF_MONTH, 13))))
res2: org.clulab.timenorm.formal.NextRI = NextRI(ThisRI(ThisRI(Year(1985,0),RepeatingField(MonthOfYear,3,Exact)),RepeatingField(DayOfMonth,15,Exact)),IntersectionRI(Set(RepeatingField(DayOfWeek,5,Exact), RepeatingField(DayOfMonth,13,Exact))),<function1>)

scala> (res2.start, res2.end)
res3: (java.time.LocalDateTime, java.time.LocalDateTime) = (1985-09-13T00:00,1985-09-14T00:00)
```
