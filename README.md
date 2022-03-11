# Spanish TimeNorm

This is a fork of the [TimeNorm SCFG](https://github.com/clulab/timenorm) normalization system that adds the **Spanish TimeNorm SFCG**. It works as the English and Italian versions, which are also included in this fork.
For instance:

```scala
scala> import org.clulab.timenorm.scfg._, scala.util.Success
import org.clulab.timenorm.scfg._
import scala.util.Success

scala> val parser = TemporalExpressionParser.es // Choose the grammar
parser: org.clulab.timenorm.scfg.TemporalExpressionParser = org.clulab.timenorm.scfg.TemporalExpressionParser@1815577b

scala> val anchor = TimeSpan.of(2013, 1, 4) // Set the anchor
anchor: org.clulab.timenorm.scfg.TimeSpan = TimeSpan(2013-01-04T00:00Z,2013-01-05T00:00Z,Period(Map(Days -> 1),Exact),Exact)

scala> val Success(temporal) = parser.parse("hace dos semanas", anchor) // Input an expression
temporal: org.clulab.timenorm.scfg.Temporal = TimeSpan(2012-12-17T00:00Z,2012-12-24T00:00Z,Period(Map(Weeks -> 1),Exact),Exact)

scala> temporal.timeMLValue // Obtain TimeML value
res0: String = 2012-W51
```

To normalize and evaluate multiple timexes from several files, use the `Evaluator`. Input file must be in "timex  \[type]  gold_value" tab-separated format, timexes from different documents should be separated by newlines and the anchor has to be the first timex of each document timexes.

```scala
scala> val evaluator = Evaluator
evaluator: Evaluator.type = Evaluator$@29f3185c

scala> evaluator.main("es", "input_file.tsv", "output_file.tsv")
```

The output of the `Evaluator` should be something like this, showing the expression, the gold value and the predicted value of each timex and the final evaluation:

```
...

2000-09-15	2000-09-15	2000-09-15
2008	2008	2008
julio del próximo año	2001-07	2001-07
hoy	2000-09-15	2000-09-15
el año 2007	2007	2007
1970	1970	1970
2002	2002	2002

Number of timexes (also DCTs):    234
Correct normalizations:           195
Incorrect normalizations:          39
Accuracy:                       83.33
```

See the original [TimeNorm](https://github.com/clulab/timenorm) page for more information.

This fork adapts the **TimeNorm SCFG** version presented in:

> Steven Bethard. 2013.
> [A Synchronous Context Free Grammar for Time Normalization](http://www.aclweb.org/anthology/D13-1078).
> In: Proceedings of the 2013 Conference on Empirical Methods in Natural
> Language Processing, pp. 821-826.
