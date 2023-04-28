# Spanish TimeNorm

This is a fork of the [TimeNorm SCFG](https://github.com/clulab/timenorm) normalization system that adds the **Spanish TimeNorm SFCG**. It works as the English and Italian versions, which are also included in this fork.

The project is related to [XTN Multilingual Timex Detection and Normalization](https://github.com/NGEscribano/XTN-timexes/tree/main), as described in

> Nayla Escribano, German Rigau and Rodrigo Agerri. 2023. [A Modular Approach for Multilingual Timex Detection and Normalization using Deep Learning and Grammar-based methods](https://arxiv.org/abs/2304.14221). arXiv:2304.14221v1.

## Preparing the environment for using TimeNorm SCFG

To use the TimeNorm SCFG system from the Scala console:

1. Clone this repository.

2. Check that your Scala version is the one in `build.sbt`.

3. Compile the program from the main directory using `sbt package`.

4. Open the Scala console using `scala -cp target/scala-[version]/timenorm_[version].jar`.

Once you open the Scala console, you should be able to use TimeNorm SCFG to normalize single or multiple timexes.

## How to normalize single timexes

To normalize a single timex, just choose the language of the grammar, set a temporal anchor and enter the timex. This provides a TimeNorm normalization value, which can be converted to TimeML format.

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

## How to normalize multiple timexes

To normalize and evaluate multiple timexes from a file, use the `Evaluator`. Input file must be in "timex  \[type]  gold_value" tab-separated format, timexes from different documents should be separated by newlines and the anchor has to be the first timex of each set of document timexes.

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

## More info

See the original [TimeNorm](https://github.com/clulab/timenorm) page for more information.

This fork adapts the **TimeNorm SCFG** version presented in

> Steven Bethard. 2013.
> [A Synchronous Context Free Grammar for Time Normalization](http://www.aclweb.org/anthology/D13-1078).
> In: Proceedings of the 2013 Conference on Empirical Methods in Natural
> Language Processing, pp. 821-826.
