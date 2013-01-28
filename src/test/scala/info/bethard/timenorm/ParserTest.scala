package info.bethard.timenorm

import scala.collection.immutable.Seq
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.threeten.bp.temporal.ChronoUnit

@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

  val grammar = Grammar.fromString("""
    [Number] ||| a ||| 1 ||| 1.0
    [Number] ||| the ||| 1 ||| 1.0
    [Number] ||| two ||| 2 ||| 1.0
    [Number] ||| three ||| 3 ||| 1.0
    [Unit] ||| day ||| DAYS ||| 1.0
    [Unit] ||| days ||| DAYS ||| 1.0
    [Unit] ||| week ||| WEEKS ||| 1.0
    [Unit] ||| weeks ||| WEEKS ||| 1.0
    [Unit] ||| month ||| MONTHS ||| 1.0
    [Unit] ||| months ||| MONTHS ||| 1.0
    [Period] ||| [Unit] ||| [Unit] ||| 1.0
    [Period] ||| [Number] [Unit] ||| [Number] [Unit] ||| 1.0
    [Period] ||| [Period,1] and [Period,2] ||| Sum [Period,1] [Period,2] ||| 1.0
    [Anchor] ||| today ||| TODAY ||| 1.0
    [Anchor] ||| yesterday ||| Minus TODAY ( Period 1 DAYS ) ||| 1.0
    [Anchor] ||| tomorrow ||| Plus TODAY ( Period 1 DAYS ) ||| 1.0
    [Anchor] ||| next [Period] ||| Plus TODAY [Period] ||| 1.0
    [Anchor] ||| last [Period] ||| Minus TODAY [Period] ||| 1.0
    [Anchor] ||| [Period] from [Anchor] ||| Plus [Anchor] [Period] ||| 1.0
    [Anchor] ||| [Period] before [Anchor] ||| Minus [Anchor] [Period] ||| 1.0
    [Anchor] ||| [Period] ago ||| Minus TODAY [Period] ||| 1.0
    """)

  test("parses simple periods") {
    val parser = new Parser(grammar)
    assert(parser(Seq("two", "weeks")) === Temporal.Period.SimplePeriod(2, ChronoUnit.WEEKS))
    assert(parser(Seq("10", "days")) === Temporal.Period.SimplePeriod(10, ChronoUnit.DAYS))
    assert(parser(Seq("a", "month")) === Temporal.Period.SimplePeriod(1, ChronoUnit.MONTHS))
  }

  test("parses complex periods") {
    val parser = new Parser(grammar)
    assert(parser(Seq("two", "weeks", "and", "a", "day")) ===
      Temporal.Period.Plus(
        Temporal.Period.SimplePeriod(2, ChronoUnit.WEEKS),
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)))
  }

  test("parses simple anchors") {
    val parser = new Parser(grammar)
    assert(parser(Seq("today")) === Temporal.Anchor.Today)
  }

  test("parses complex anchors") {
    val parser = new Parser(grammar)
    assert(parser(Seq("tomorrow")) ===
      Temporal.Anchor.Plus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)))
    assert(parser(Seq("yesterday")) ===
      Temporal.Anchor.Minus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)))
    assert(parser(Seq("next", "week")) ===
      Temporal.Anchor.Plus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.WEEKS)))
    assert(parser(Seq("last", "week")) ===
      Temporal.Anchor.Minus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.WEEKS)))
    assert(parser(Seq("next", "month")) ===
      Temporal.Anchor.Plus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(1, ChronoUnit.MONTHS)))
    assert(parser(Seq("two", "weeks", "ago")) ===
      Temporal.Anchor.Minus(
        Temporal.Anchor.Today,
        Temporal.Period.SimplePeriod(2, ChronoUnit.WEEKS)))
    assert(parser(Seq("the", "day", "before", "yesterday")) ===
      Temporal.Anchor.Minus(
        Temporal.Anchor.Minus(
          Temporal.Anchor.Today,
          Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)),
        Temporal.Period.SimplePeriod(1, ChronoUnit.DAYS)))
  }

  /*
   * More things to test:
   * 
   * == Anchors ==
   * October 15, 2011
   * October 15
   * last week
   * three days from now
   * a week from yesterday
   * next October 15
   * last Friday the 13th
   * 
   * == Ranges ==
   * the next two days
   */
}
