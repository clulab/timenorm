A library for converting natural language expressions of dates and times
to their normalized form.

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

## RNN character based parser

Part of the code in this library parses time expressions using
a character-based recurrent neural network, as described in:

> Egoitz Laparra, Dongfang Xu, and Steven Bethard. 2018.	
> [From Characters to Time Intervals: New Paradigms for Evaluation and Neural Parsing of Time Normalizations.](https://www.mitpressjournals.org/doi/pdf/10.1162/tacl_a_00025)
> In: Transactions of the Association for Computational Linguistics 2018, Vol. 6, pp. 343–356

> Dongfang Xu, Egoitz Laparra and Steven Bethard. 2019.
> Pre-trained Contextualized Character Embeddings Lead to Major Improvements in Time Normalization: a Detailed Analysis.
> In: Proceedings of The Eighth Joint Conference on Lexical and Computational Semantics.

The parser is contained by the `TemporalNeuralParser` class:

```scala
scala> import org.clulab.timenorm.neural._
import org.clulab.timenorm.neural._

scala> // Create a new parser
scala> val parser = new TemporalNeuralParser()
parser: org.clulab.timenorm.neural.TemporalNeuralParser = org.clulab.timenorm.neural.TemporalNeuralParser@9b21bd3

scala> // Optionally, parse and obtain an anchor time that will be required as reference to normalize some time expressions.
scala> val anchor = parser.dct(parser.parse(List("2019-05-30"))(0))
anchor: org.clulab.timenorm.formal.Interval = ThisRI(Year(2019,0),IntersectionRI(Set(RepeatingField(MonthOfYear,5,Exact), RepeatingField(DayOfMonth,30,Exact))))

scala> // Parse and normalize a List (batch) of utterances.
scala> val timex_batch = List("since last year", "within two weeks")
scala> val temporal = parser.intervals(parser.parse(timex_batch), Some(anchor))
temporal: List[List[org.clulab.timenorm.neural.TimeExpression]] = List(List(TimeExpression(Span(0,15),List(TimeInterval(2018-01-01T00:00,2019-05-31T00:00,44496000)))), List(TimeExpression(Span(1,16),List(TimeInterval(2019-06-10T00:00,2019-06-17T00:00,604800)))))

scala> // For each element in the batch a List with the found time expressions is returned, idenfied by their character span (offsets).
scala> (temporal(0)(0).span.start, temporal(0)(0).span.end)
res1: (Int, Int) = (0,15)

scala> // If it succeeds, a time expression is normalized into a List of time intervals. For each time interval the start time, end time and duration (in seconds) are returned.
scala> (temporal(0)(0).intervals(0).start, temporal(0)(0).intervals(0).end, temporal(0)(0).intervals(0).duration)
res2: (java.time.LocalDateTime, java.time.LocalDateTime, Long) = (2018-01-01T00:00,2019-05-31T00:00,44496000)
```

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

