package org.clulab

/**
 * This package provides classes for parsing natural language expressions into normalized forms.
 * 
 * The primary entry point for most users is [[TemporalExpressionParser]], which parses a
 * [[Temporal]] from a string:
 * {{{
    // create a new parser (using the default English grammar)
    val parser = new TemporalExpressionParser
    // establish an anchor time
    val anchor = TimeSpan.of(2013, 1, 4)
    // parse an expression given an anchor time (assuming here that it succeeds)
    val Success(temporal) = parser.parse("two weeks ago", anchor)
    // get the TimeML value ("2012-W51") from the Temporal
    val value = temporal.timeMLValue
 * }}}
 */
package object timenorm {
}
