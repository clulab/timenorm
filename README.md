# timenorm

The timenorm library provides models for finding natural language expressions
of dates and times and converting them to a normalized form.

## Text to time expressions with the neural parser

The primary entry point for the library is the `TemporalNeuralParser` class,
which implements a character-based recurrent neural network for finding and
normalizing time expressions, as described in:

> Egoitz Laparra, Dongfang Xu, and Steven Bethard. 2018.	
> [From Characters to Time Intervals: New Paradigms for Evaluation and Neural Parsing of Time Normalizations](https://www.mitpressjournals.org/doi/pdf/10.1162/tacl_a_00025).
> In: Transactions of the Association for Computational Linguistics 2018, Vol. 6, pp. 343–356

> Dongfang Xu, Egoitz Laparra and Steven Bethard. 2019.
> [Pre-trained Contextualized Character Embeddings Lead to Major Improvements in Time Normalization: a Detailed Analysis](https://www.aclweb.org/anthology/S19-1008).
> In: Proceedings of The Eighth Joint Conference on Lexical and Computational Semantics.

To use the parser, create an instance of `TemporalNeuralParser`, and provide
as an anchor the time at which your text was written.

```scala
scala> import org.clulab.timenorm.scate._
import org.clulab.timenorm.scate._

scala> val parser = new TemporalNeuralParser
parser: org.clulab.timenorm.scate.TemporalNeuralParser = org.clulab.timenorm.scate.TemporalNeuralParser@44c2e8a8

scala> val anchor = SimpleInterval.of(2019, 5, 30)
anchor: org.clulab.timenorm.scate.SimpleInterval = SimpleInterval(2019-05-30T00:00,2019-05-31T00:00)
```

When you pass text to the parser, it will return the predicted time
expressions. Each time expression contains the span of characters that evoked
the time expression. Interval-type time expressions also contain their start
and endpoints on the timeline. 

```scala
scala> val text = "I have not seen her since last year. We hope to meet in the next two weeks."
text: String = I have not seen her since last year. We hope to meet in the next two weeks.
scala> for (timex <- parser.parse(text, anchor)) timex match {
     |   case interval: Interval =>
     |     val Some((charStart, charEnd)) = interval.charSpan
     |     println(s"${interval.start} ${interval.end} ${text.substring(charStart, charEnd)}")
     | }

2018-01-01T00:00 2019-05-31T00:00 since last year
2019-05-31T00:00 2019-06-14T00:00 next two weeks
```

The parser runs faster if you pass batches of texts via the `parseBatch`
(instead of the `parse`) method. So if you know, say, the sentence segmentation
for your text, you may prefer to use that method.

## Semantically compositional time operators

The `TimeExpression` objects returned by the neural parser are based on the set
of semantically compositional operators described in:

> Steven Bethard and Jonathan Parker. 2016.
> [A Semantically Compositional Annotation Scheme for Time Normalization](http://www.lrec-conf.org/proceedings/lrec2016/pdf/288_Paper.pdf).
> In: Proceedings of the Tenth International Conference on Language
> Resources and Evaluation (LREC 2016). pp. 3779-3786.

If you would like to manually construct complex time expressions, the operators
are available in the `org.clulab.timenorm.scate` package, and can be combined
with fields and units from the `java.time` library:

```scala
scala> import org.clulab.timenorm.scate._, java.time.temporal.ChronoField._, java.time.temporal.ChronoUnit._
import org.clulab.timenorm.scate._
import java.time.temporal.ChronoField._
import java.time.temporal.ChronoUnit._

scala> // the 3-year period following the year 1985
scala> NextP(Year(1985), SimplePeriod(YEARS, 3))
res0: org.clulab.timenorm.scate.NextP = NextP(Year(1985,0,None),SimplePeriod(Years,IntNumber(3,None),None,None),None)

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
res2: org.clulab.timenorm.scate.NextRI = NextRI(ThisRI(ThisRI(Year(1985,0,None),RepeatingField(MonthOfYear,3,None,None),None),RepeatingField(DayOfMonth,15,None,None),None),IntersectionRI(Set(RepeatingField(DayOfWeek,5,None,None), RepeatingField(DayOfMonth,13,None,None)),None),<function1>,None)

scala> (res2.start, res2.end)
res3: (java.time.LocalDateTime, java.time.LocalDateTime) = (1985-09-13T00:00,1985-09-14T00:00)
```

## Normalizing time expressions with a synchronous context free grammar

*This portion of the library is no longer recommended, but it is still included
in the distribution.*

The `scfg` portion of the library can take a time expression and normalize it
to TimeML format using a synchronous context free grammar, as described in:

> Steven Bethard. 2013.
> [A Synchronous Context Free Grammar for Time Normalization](http://www.aclweb.org/anthology/D13-1078).
> In: Proceedings of the 2013 Conference on Empirical Methods in Natural
> Language Processing, pp. 821-826.

Note that the `scfg` model cannot find time expressions in text; it can only
normalize them after they are found. Sample usage:

```scala
scala> import org.clulab.timenorm.scfg._, scala.util.Success
import org.clulab.timenorm.scfg._
import scala.util.Success

scala> val parser = TemporalExpressionParser.en // English, Italian, and also Spanish are available.
parser: org.clulab.timenorm.scfg.TemporalExpressionParser = org.clulab.timenorm.scfg.TemporalExpressionParser@d653e41

scala> val Success(temporal) = parser.parse("two weeks ago", TimeSpan.of(2013, 1, 4))
temporal: org.clulab.timenorm.scfg.Temporal = TimeSpan(2012-12-17T00:00Z,2012-12-24T00:00Z,Period(Map(Weeks -> 1),Exact),Exact)

scala> temporal.timeMLValue
res0: String = 2012-W51
```

### Languages

The `TemporalExpressionParser` is available in three languages: English (en), Italian (it), Spanish (es).  The last is thanks to contributions of [@NGEscribano ](https://github.com/NGEscribano) from the [timenorm-es](https://github.com/NGEscribano/timenorm-es) project which includes additional helpful information in its [Spanish TimeNorm](https://github.com/NGEscribano/timenorm-es#readme) document.
