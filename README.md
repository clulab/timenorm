The adaptation of TimeNorm, a library for converting natural language expressions of dates and times to
their normalized form based on synchronous context free grammars, for Italian language.

The primary entry point for most users is `TemporalExpressionParser`, which
parses a `Temporal` from a string:

```scala
// create a new parser (using the default English grammar)
val parser = new TemporalExpressionParser
// establish an anchor time
val anchor = TimeSpan.of(2013, 1, 4)
// parse an expression given an anchor time (here, assuming it succeeds)
val Success(temporal) = parser.parse("two weeks ago", anchor)
// get the TimeML value ("2012-W51") from the Temporal
val value = temporal.timeMLValue
```

The library is available on Maven central:

```xml
<dependency>
  <groupId>info.bethard</groupId>
  <artifactId>timenorm</artifactId>
  <version>0.9.0</version>
</dependency>
```

You can also just dowload the .jar directly:

https://github.com/paramitamirza/timenorm/releases/download/timenorm-it-0.9.2/timenorm-it-0.9.2.jar
 
