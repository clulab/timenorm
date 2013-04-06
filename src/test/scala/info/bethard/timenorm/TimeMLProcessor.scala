package info.bethard.timenorm

import java.io.File

import scala.collection.JavaConverters._

import org.threeten.bp.DateTimeException

import com.lexicalscope.jewel.cli.CliFactory
import com.lexicalscope.jewel.cli.{ Option => CliOption }

/**
 * This is not actually a test, but it can be run over TimeML files to see what can and cannot
 * be parsed. To increase coverage, new rules can be added to the timenorm.grammar resource.
 */
object TimeMLProcessor {

  private final val annotationErrors = Set(
    ("APW19980811.0474.tml", "t5", "last week", "1998-08-04"),
    ("APW19980818.0515.tml", "t7", "last week", "P1W"),
    ("APW19980820.1428.tml", "t1", "Friday", "1998-08-07"),
    ("APW19980820.1428.tml", "t2", "this month", "P1M"),
    ("APW19980826.0389.tml", "t4", "September", "P1M"),
    ("APW19981205.0374.tml", "t0", "12/05/1998 09:42:00", "1998-12-05T09:42"),
    ("APW19981205.0374.tml", "t3", "early this week", "1998-12-XX"),
    ("APW19990122.0193.tml", "t0", "1999-01-22 13:06:18", "1999-01-22T13:06"),
    ("APW19990122.0193.tml", "t3", "last year", "1998-10"),
    ("APW19990122.0193.tml", "t5", "last three years", "P1Y"),
    ("APW19990122.0193.tml", "t7", "Last week", "1999-01"),
    ("APW19990122.0193.tml", "t8", "weeks", "1998-10"),
    ("APW19990206.0090.tml", "t0", "1999-02-06 06:22:26", "1999-02-06T06:22"),
    ("APW19990206.0090.tml", "t3", "this week", "1999-02"),
    ("APW19990206.0090.tml", "t6", "that same day.", "1998-10-XX") /* no anchor given */,
    ("APW19990216.0198.tml", "t0", "1999-02-16 12:55:33", "1999-02-16T12:55"),
    ("APW19990216.0198.tml", "t4", "50 years ago", "1949-XX"),
    ("APW19990312.0251.tml", "t0", "1999-03-12 10:34:13", "1999-03-12T10:34"),
    ("APW19990312.0251.tml", "t4", "less than a decade", "PXY"),
    ("APW19990312.0251.tml", "t6", "a decade ago", "1989-XX"),
    ("APW19990312.0251.tml", "t7", "1968", "1968-XX"),
    ("APW19990312.0251.tml", "t9", "1949", "1949-XX"),
    ("APW19990312.0251.tml", "t10", "1952", "1952-XX"),
    ("APW19990312.0251.tml", "t11", "1955", "1955-XX"),
    ("APW19990312.0251.tml", "t12", "1982", "1982-XX"),
    ("APW19990312.0251.tml", "t14", "1949", "1949-XX"),
    ("APW19990312.0251.tml", "t17", "A decade ago", "1989-XX"),
    ("APW19990312.0251.tml", "t20", "this century", "P100Y"),
    ("APW19990410.0123.tml", "t0", "1999-04-10 06:27:43", "1999-04-10T06:27"),
    ("APW19990410.0123.tml", "t5", "days", "1998-FA"),
    ("APW19990506.0155.tml", "t0", "1999-05-06 15:05:11", "1999-05-06T15:05"),
    ("APW19990506.0155.tml", "t7", "weeks", "1998-10"),
    ("APW19990507.0207.tml", "t0", "1999-05-07 03:44:53", "1999-05-07T03:44"),
    ("APW19990507.0207.tml", "t10", "weeks", "1998-FA"),
    ("APW19990607.0041.tml", "t0", "1999-06-07 19:00:11", "1999-06-07T19:00"),
    ("APW19991008.0151.tml", "t5", "Thursday morning", "1999-10-07"),
    ("APW19991024.0075.tml", "t0", "1999-10-24 20:00:09", "1999-10-24T20:00"),
    ("APW19991024.0075.tml", "t5", "tonight", "1999-10-24"),
    ("APW19991024.0075.tml", "t8", "Earlier that day", "1998-10-23") /* no anchor given */,
    ("APW199980817.1193.tml", "t3", "Tuesday morning", "1998-08-18"),
    ("APW199980817.1193.tml", "t8", "the weekend", "P2D"),
    ("APW199980817.1193.tml", "t10", "last week", "P1W"),
    ("APW20000106.0064.tml", "t1", "Wednesday night", "2000-01-05"),
    ("APW20000107.0088.tml", "t10", "Wednesday night", "2000-01-05TEV"),
    ("APW20000107.0318.tml", "t9", "early next week", "2000-W2") /* format should be Wxx */,
    ("APW20000107.0318.tml", "t10", "Friday night", "2000-01-07TEV"),
    ("APW20000107.0318.tml", "t15", "Friday night", "2000-01-07"),
    ("APW20000115.0031.tml", "t7", "this week", "P1W"),
    ("APW20000115.0031.tml", "t10", "decades-long", "PXY"),
    ("APW20000115.0209.tml", "t1", "Thursday", "2000-01-06") /* DCT is wrong */,
    ("APW20000115.0209.tml", "t5", "Wednesday", "2000-01-05") /* DCT is wrong */,
    ("APW20000115.0209.tml", "t8", "Wednesday", "2000-01-05") /* DCT is wrong */,
    ("APW20000115.0209.tml", "t10", "Wednesday night", "2000-01-05TEV"),
    ("APW20000115.0209.tml", "t12", "Wednesday", "2000-01-05") /* DCT is wrong */,
    ("APW20000128.0316.tml", "t22", "later that year", "1998") /* no anchor given */,
    ("APW20000210.0328.tml", "t2", "last week", "2000-W7"),
    ("APW20000210.0328.tml", "t4", "this week", "2000-W6") /* format should be Wxx */,
    ("APW20000216.0193.tml", "t4", "this week", "2000-W7") /* format should be Wxx */,
    ("APW20000216.0272.tml", "t6", "this week", "2000-W7") /* format should be Wxx */,
    ("APW20000401.0150.tml", "t2", "more than four decades ago", "P40Y"),
    ("APW20000403.0057.tml", "t3", "Tuesday", "2000-04-05"),
    ("APW20000405.0276.tml", "t4", "4 in the morning", "2000-04-06T04:00") /* no anchor given */,
    ("APW20000405.0276.tml", "t10", "Wednesday night", "2000-04-05"),
    ("APW20000417.0031.tml", "t3", "Sunday night", "2000-04-16"),
    ("APW20000417.0031.tml", "t6", "Sunday night", "2000-04-16"),
    ("NYT19980907.0112.tml", "t11", "years-old", "P1Y"),
    ("NYT19980907.0112.tml", "t9", "13 years", "P12Y"),
    ("NYT19981025.0188.tml", "t3", "past decade", "P10Y"),
    ("NYT19981025.0188.tml", "t5", "Last week", "1998-10"),
    ("NYT19981025.0188.tml", "t8", "week", "1998-01"),
    ("NYT19981025.0216.tml", "t5", "1992", "1992-05-10"),
    ("NYT19981025.0216.tml", "t7", "1980s", "198X"),
    ("NYT19981025.0216.tml", "t8", "1990s", "199X"),
    ("NYT19981025.0216.tml", "t9", "1992", "1992-10-02"),
    ("NYT19981026.0446.tml", "t6", "last week", "1998-10-XX"),
    ("NYT19981026.0446.tml", "t7", "earlier this year", "1998-XX-XX"),
    ("NYT19981026.0446.tml", "t13", "first day", "1999-01-XX") /* "first day in office" ??? */,
    ("NYT19981026.0446.tml", "t14", "next week", "1998-11-03"),
    ("NYT19981120.0362.tml", "t7", "days", "1998-10-XX"),
    ("NYT19981121.0173.tml", "t2", "last week", "1998-11"),
    ("NYT19981121.0173.tml", "t11", "day", "1998-10-24"),
    ("NYT19990312.0271.tml", "t4", "1948", "1948-XX"),
    ("NYT19990312.0271.tml", "t6", "10 years ago", "1989-XX"),
    ("NYT19990419.0515.tml", "t4", "next week.", "1999-04-XX"),
    ("NYT19990419.0515.tml", "t9", "6 a.m. ten days", "1998-10-13T06:00"),
    ("NYT19990419.0515.tml", "t16", "days", "199X-XX-XX"),
    ("NYT19990419.0515.tml", "t14", "1994", "1994-XX-XX"),
    ("NYT19990419.0515.tml", "t11", "1995", "1995-XX-XX"),
    ("NYT19990419.0515.tml", "t12", "1997", "1997-XX-XX"),
    ("NYT19990419.0515.tml", "t15", "10 p.m", "1998-10-23T22:00") /* no anchor given */,
    ("NYT19990505.0443.tml", "t8", "days", "1998-10"),
    ("NYT19990505.0443.tml", "t10", "6 a.m", "1998-10-13T06:00") /* no anchor given */,
    ("NYT19990505.0443.tml", "t11", "10 days", "1998-10-13"),
    ("NYT20000224.0173.tml", "t3", "this week", "2000-W8") /* format should be Wxx */,
    ("NYT20000224.0173.tml", "t5", "1960s", "196X"),
    ("NYT20000224.0173.tml", "t10", "early 1990s", "199X"),
    ("NYT20000224.0173.tml", "t16", "the next morning", "2000-02-23"),
    ("NYT20000329.0359.tml", "t4", "Thursday morning", "2000-03-30"),
    ("NYT20000330.0406.tml", "t1", "Tuesday morning", "2000-04-04"),
    ("NYT20000403.0463.tml", "t20", "9 a.m. EST Tuesday", "2000-04-03T09:00") /* not a Tuesday */,
    ("NYT20000403.0463.tml", "t21", "the weekend", "P2D"),
    ("NYT20000424.0319.tml", "t4", "the weekend", "2000-04-22"),
    ("NYT20000601.0442.tml", "t5", "six months", "P6D"),
    ("XIE19980808.0031.tml", "t3", "Friday morning", "1998-08-07"),
    ("XIE19980814.0294.tml", "t0", "August 13", "1998-08-14"),
    ("XIE19990210.0079.tml", "t3", "Wednesday", "1999-02-09") /* not a Wednesday */,
    ("XIE19990210.0079.tml", "t6", "25", "1999-04-25") /* "April 24 and 25" ??? */,
    ("XIE19990210.0079.tml", "t9", "Tuesday", "1999-02-08") /* not a Tuesday */,
    ("XIE19990313.0031.tml", "t3", "day", "1999-03-12") /* "a great day" - no anchor given */,
    ("XIE19990313.0229.tml", "t3", "day", "1999-03-12") /* "a great day" - no anchor given */,
    ("XIE19990313.0229.tml", "t4", "this century,", "P100Y"),
    ("ABC19980108.1830.0711.tml", "t85", "the last twenty four hours", "P1D"),
    ("AP900815-0044.tml", "t47", "some time", "PXM") /* should be PXX if the other PXX annotations are right */,
    ("AP900815-0044.tml", "t188", "Tuesday", "1990-08-07") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t204", "Tuesday", "1990-08-07") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t210", "Monday", "1990-08-06") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t252", "Tuesday", "1990-08-07") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t269", "Tuesday", "1990-08-07") /* wrong anchor: anchor is more than a week after "value" */,
    ("AP900815-0044.tml", "t276", "eighth day", "1990-08-15") /* eighth day of Desert Storm, but anchor is not first day of Desert Storm */,
    ("AP900816-0139.tml", "t352", "A day earlier", "P1D") /* should be date, not period (and no anchor) */,
    ("APW19980213.1310.tml", "t117", "several years ago", "PAST_REF") /* TIDES spec says, e.g. "several weeks ago" => PAST_REF */,
    ("APW19980219.0476.tml", "t130", "almost two years ago", "P2Y") /* should be date, not period */,
    ("APW19980219.0476.tml", "t169", "July last year", "1997-06") /* July is 07 */,
    ("APW19980227.0494.tml", "t154", "centuries", "PXE") /* centuries are CE */,
    ("APW19980301.0720.tml", "t1989", "six weeks ago", "1998-W02") /* ISO weeks start on Monday, not Sunday */,
    ("APW19980306.1001.tml", "t1000", "daily", "XXXX-XX-XX") /* TimeML spec says "daily" is P1D */,
    ("APW19980308.0201.tml", "t71", "recent days", "PAST_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("APW19980322.0749.tml", "t2023", "two weeks ago", "1998-03-08") /* possible interpretation, but usually these are -WXX values */,
    ("APW19980322.0749.tml", "t138", "Sunday", "1998-02-22") /* wrong since ref time is 1998-03-22 */,
    ("CNN19980213.2130.0155.tml", "t145", "next week", "1998-WXX") /* XX should be a number (the week is identifiable) */,
    ("CNN19980227.2130.0067.tml", "t108", "this week", "1998-WXX") /* XX should be a number (the week is identifiable) */,
    ("NYT19980206.0460.tml", "t201", "coming months", "FUTURE_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("NYT19980206.0460.tml", "t1002", "two decades", "P20Y") /* decades are DE */,
    ("NYT19980206.0460.tml", "t213", "the last few months", "PAST_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("NYT19980212.0019.tml", "t47", "a few years ago", "199X") /* TIDES spec says, e.g. "several weeks ago" => PAST_REF */,
    ("PRI19980121.2000.2591.tml", "t1982", "centuries", "PXC") /* centuries are CE */,
    ("PRI19980121.2000.2591.tml", "t1986", "a few minutes", "PXM") /* PXM is months, PXTM is minutes */,
    ("PRI19980121.2000.2591.tml", "t1991", "more than two thousand years", "P2L") /* L is not a type; should be 2000Y */,
    ("PRI19980306.2000.1675.tml", "t31", "the second day", "1998-03-06") /* "second day of an offensive", but anchor is not first day of the offensive */,
    ("VOA19980303.1600.2745.tml", "t116", "this year", "1998") /* wrong anchor: should be doctime but is "last year" */,
    ("VOA19980305.1800.2603.tml", "t87", "today", "PRESENT_REF") /* "today" should be a date */,
    ("VOA19980331.1700.1533.tml", "t105", "a year or two", "FUTURE_REF") /* should probably be "PXY" */,
    ("WSJ900813-0157.tml", "t456", "the end of the month", "1990-08-30") /* should be 1990-08 with END modifier */,
    ("WSJ910225-0066.tml", "t496", "a week or so ago", "1991-02-18") /* possible interpretation, but usually these are -WXX values */,
    ("WSJ910225-0066.tml", "t507", "noon Saturday", "1991-02-16T12:00") /* wrong anchor: anchor is more than a week after "value" */,
    ("WSJ910225-0066.tml", "t514", "Saturday night", "1991-02-16TNI") /* wrong anchor: anchor is more than a week after "value" */,
    ("wsj_0006.tml", "t10", "year-end", "1989-12-31") /* should be 1989 with an END modifier */,
    ("wsj_0068.tml", "t134", "the year-ago quarter", "1988-Q3") /* wrong anchor: annotated anchor is in November */,
    ("wsj_0124.tml", "t36", "Sept. 27, 1989", "1989-11-27") /* September is 9, not 11 */,
    ("wsj_0127.tml", "t27", "1988", "1998") /* 1998 != 1988 */,
    ("wsj_0136.tml", "t2027", "fourth quarter", "1998-Q4") /* document is written in 1989, not 1998 */,
    ("wsj_0136.tml", "t2023", "Sept. 30", "1989-09") /* missing the days */,
    ("wsj_0136.tml", "t2024", "the fiscal year", "1989") /* should be P1Y, as it is in most other places */,
    ("wsj_0136.tml", "t37", "fiscal 1988", "1989") /* 1998 != 1989 */,
    ("wsj_0144.tml", "t52", "the latest quarter", "1989-Q3") /* wrong anchor: should be doctime but is "third-quarter" */,
    ("wsj_0144.tml", "t60", "year-ago", "1988") /* anchor is quarter, so either value should be quarter or anchor should be doctime */,
    ("wsj_0157.tml", "t29", "five years", "1994") /* wrongly interpreted as "in five years" */,
    ("wsj_0168.tml", "t23", "one-year term", "P1Y") /* "term" should not be part of the expression */,
    ("wsj_0176.tml", "t23", "the next three quarters", "P9M") /* should be P3Q */,
    ("wsj_0176.tml", "t25", "recent quarters", "PXM") /* should be PXQ */,
    ("wsj_0189.tml", "t42", "the latest period", "1989-Q4") /* doc is in November, so latest period should be Q3 */,
    ("wsj_0266.tml", "t39", "about two years ago", "P2Y") /* "ago" means it should be 1987 */,
    ("wsj_0266.tml", "t40", "the 20th century", "19XX") /* should be 19 */,
    ("wsj_0325.tml", "t51", "year-ago", "1988-Q3") /* anchor is wrong (points to year-ago quarter, not current one) */,
    ("wsj_0340.tml", "t61", "last week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0348.tml", "t158", "a year ago", "1988-11") /* should be either 1988 or, as in WSJ articles, 1988-Q3 */,
    ("wsj_0348.tml", "t55", "a year", "1988-Q3") /* annotation missed trailing "earlier" */,
    ("wsj_0520.tml", "t92", "this year", "1989-10-30") /* should either be 1989 or annotation should include preceding "so far" */,
    ("wsj_0527.tml", "t122", "a year ago", "1988-Q3") /* wrong anchor: is doc time, but should be the quarter */,
    ("wsj_0533.tml", "t124", "last week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0542.tml", "t87", "5 p.m. EST, Nov. 6", "1989-11-06T17") /* all other am/pm references add the :00 */,
    ("wsj_0542.tml", "t90", "midnight Friday", "1989-10-27T24") /* TIDES spec says this should be T24:00 */,
    ("wsj_0542.tml", "t102", "Late last week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0551.tml", "t17", "around year end", "1989-12") /* should be 1989 with an END modifier */,
    ("wsj_0558.tml", "t57", "nearly two weeks ago", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0568.tml", "t237", "future quarters", "FUTURE_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("wsj_0568.tml", "t249", "the past several quarters", "PAST_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("wsj_0568.tml", "t2013", "a year", "1988") /* annotation missed trailing "earlier" */,
    ("wsj_0575.tml", "t213", "that time", "P5Y") /* no anchor, but one is necessary */,
    ("wsj_0575.tml", "t238", "Last week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0583.tml", "t217", "recent years", "PAST_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("wsj_0585.tml", "t320", "past years", "PAST_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("wsj_0585.tml", "t357", "the week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0586.tml", "t215", "the previous Friday", "1989-10-20") /* wrong anchor: is doc time, but should be before doc time */,
    ("wsj_0586.tml", "t226", "the week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0586.tml", "t293", "the week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0586.tml", "t341", "year-end", "1988-12-31") /* should be 1988 with an END modifier */,
    ("wsj_0675.tml", "t311", "this week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0745.tml", "t2007", "This week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0760.tml", "t60", "the year-earlier period", "P9M") /* no anchor, but one is necessary (and probably should be a YYYY-QN too) */,
    ("wsj_0768.tml", "t174", "Last week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0768.tml", "t196", "recent years", "PAST_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("wsj_0768.tml", "t199", "last week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0768.tml", "t201", "week", "1989-WXX") /* XX should be a number (the week is identifiable); also, annotation is missing "the" */,
    ("wsj_0778.tml", "t235", "this week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0781.tml", "t278", "sometime next year", "1990-XX-XX") /* should probably just be 1990 */,
    ("wsj_0781.tml", "t279", "last week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0781.tml", "t281", "last week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0781.tml", "t286", "next week", "1989-WXX") /* XX should be a number (the week is identifiable) */,
    ("wsj_0798.tml", "t1000", "annually", "XXXX") /* TimeML spec says, e.g. "daily" is P1D */,
    ("wsj_0805.tml", "t77", "later this year", "1989-11-01") /* actually a legitimate annotation (using ON_OR_AFTER), but inconsistent with other annotations use END */,
    ("wsj_0927.tml", "t98", "Quarterly", "P3M") /* TIDES spec says quarters are PnQX (but should at least be PnQ as in other TimeBank annotations) */,
    ("wsj_0928.tml", "t73", "a year earlier", "1988-QX") /* should be 1988-Q3 (anchor's value is also wrong) */,
    ("wsj_0928.tml", "t83", "April", "1988-04") /* should be 1989-04 (doctime is 1989-10-26) */,
    ("wsj_0950.tml", "t97", "a year ago", "1988-10") /* should be a day or a quarter (with a different anchor), not a month */,
    ("wsj_0973.tml", "t141", "about a decade", "P1E") /* decade is DE, not E */,
    ("wsj_1003.tml", "t190", "coming quarters", "FUTURE_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("wsj_1003.tml", "t236", "quarter", "1989-QX") /* annotation is missing "the" */,
    ("wsj_1003.tml", "t239", "a year ago", "1988-QX") /* X should be a number (the quarter is identifiable) */,
    ("wsj_1003.tml", "t240", "The latest period", "1989-Q2") /* wrong anchor: should be doctime, but is another quarter */,
    ("wsj_1011.tml", "t65", "the quarter", "1989-QX") /* X should be a number (the quarter is identifiable) */,
    ("wsj_1011.tml", "t70", "the quarter", "1989-QX") /* X should be a number (the quarter is identifiable) */,
    ("wsj_1011.tml", "t74", "the quarter", "1989-QX") /* X should be a number (the quarter is identifiable) */,
    ("wsj_1013.tml", "t166", "the latest quarter", "1989-QX") /* X should be a number (the quarter is identifiable) */,
    ("wsj_1013.tml", "t169", "recent years", "PAST_REF") /* TIDES spec says, e.g. "recent decades" => PXDE */,
    ("wsj_1025.tml", "t38", "the latest period", "1989-QX") /* X should be a number (the quarter is identifiable) */,
    ("wsj_1033.tml", "t167", "the year-ago period", "1988-Q3") /* wrong anchor: should be another quarter, but is doctime */)

  private final val knownFailures = Set(
    ("NYT20000414.0296.tml", "t5", "the last week", "2000-W15") /* interpreted as "this week" */,
    ("APW19980322.0749.tml", "t134", "last week", "1998-W12") /* interpreted as "this week" */,
    ("APW19980808.0022.tml", "t4", "10:35 a.m.", "1998-08-07T10:35") /* document creation time is whole day, so FindAtOrEarlier finds the one today, not yesterday */,
    ("APW19980213.1320.tml", "t190", "Monday", "XXXX-WXX-1TNI") /* "Monday and Tuesday nights" */,
    ("APW19980219.0476.tml", "t137", "weeks or months", "PXW") /* requires handling of disjunctions */,
    ("APW19980301.0720.tml", "t1982", "last February", "1997-02") /* two Februaries before anchor */,
    ("PRI19980303.2000.2550.tml", "t163", "one day", "FUTURE_REF") /* ambiguous with P1D */,
    ("VOA19980305.1800.2603.tml", "t66", "this coming Sunday, March eighth", "1998-03-08") /* need handling of ordinals */,
    ("VOA19980331.1700.1533.tml", "t3000", "two", "P2D") /* from "two to six days" */,
    ("wsj_0144.tml", "t59", "the 1988 period", "1988") /* requires handling of corresponding quarters (this is also annotated wrong; should be a quarter) */,
    ("wsj_0171.tml", "t32", "the quarter", "1989-Q3") /* interpreted as "the last quarter" */,
    ("wsj_0263.tml", "t2096", "A year earlier", "1988-11-01") /* ambiguous with 1988 */,
    ("wsj_0292.tml", "t86", "the 1989 period", "1989-Q3") /* "quarter-to-quarter comparison" */,
    ("wsj_0534.tml", "t22", "then", "1989-11-30") /* should be handled like PRESENT but not display PRESENT_REF */,
    ("wsj_0570.tml", "t88", "from time to time", "FUTURE_REF") /* occurs only once (might be better interpreted as a set instead) */,
    ("wsj_0585.tml", "t1111", "two to three weeks", "P2W") /* requires handling of disjunctions */,
    ("wsj_0585.tml", "t386", "the 1988 quarter", "1988-Q3") /* requires handling of corresponding quarters */,
    ("wsj_0637.tml", "t46", "the next 12 to 18 months", "PXM") /* requires handling of disjunctions */,
    ("wsj_0662.tml", "t36", "the quarter", "1989-Q2") /* really means "last quarter" */,
    ("wsj_0709.tml", "t30", "the comparable year-ago quarter", "1988-Q2") /* requires handling of corresponding quarters */,
    ("wsj_0768.tml", "t2004", "recent weeks and months", "PAST_REF") /* requires handling of conjunctions of unspecified periods */,
    ("wsj_0810.tml", "t167", "last", "1988") /* "...this year as last" */,
    ("wsj_0904.tml", "t247", "the year-earlier nine months", "P9M") /* not sure how to capture this */,
    ("wsj_0918.tml", "t200", "the quarter", "1989-Q3") /* really means "last quarter" */,
    ("wsj_0928.tml", "t74", "the 1988 period", "1988-Q1") /* requires handling of corresponding quarters */,
    ("wsj_1003.tml", "t183", "first two quarters of 1990", "1990-H1") /* grammar could be adjusted to handle this, but would normalize it to 1990-01-01 + 2 QUARTERS */,
    ("wsj_1003.tml", "t211", "a year ago", "1988-10-26") /* ambiguous with 1988 */,
    ("wsj_1003.tml", "t215", "a year ago", "1988-10-26") /* ambiguous with 1988 */,
    ("wsj_1011.tml", "t60", "last spring", "1988-SP") /* interpreted as the spring of the last year, but ambiguous with the spring earlier this year */,
    ("wsj_1013.tml", "t167", "a year earlier", "1988-QX") /* annotation is correct given anchor, but anchor should not be QX */,
    ("wsj_1014.tml", "t283", "at least the past 18 months", "P18M") /* modifier goes on Period but is syntactically attached to TimeSpan */,
    ("wsj_1014.tml", "t371", "the last half of 1989", "1989-H2") /* currently no handling of half-TimeSpans */,
    ("wsj_1014.tml", "t446", "the last half of the '80s", "198") /* currently no handling of half-TimeSpans */,
    ("wsj_1025.tml", "t40", "second", "XXXX-Q2") /* "second and fourth quarters */)

  trait Options {
    @CliOption(longName = Array("corpus-paths"))
    def getCorpusPaths: java.util.List[File]
    @CliOption(longName = Array("fail-on-no-correct-parse"))
    def getFailOnNoCorrectParse: Boolean
  }

  def main(args: Array[String]): Unit = {
    val options = CliFactory.parseArguments(classOf[Options], args: _*)
    def error(message: String, timex: TimeMLDocument#TimeExpression, file: File) = {
      printf("%s \"%s\" (%s) from %s\n", message, timex.text, timex.value, file)
    }
    def fatal(message: String, timex: TimeMLDocument#TimeExpression, file: File, cause: Throwable) = {
      if (options.getFailOnNoCorrectParse) {
        val exceptionMessage = String.format(
          "%s \"%s\" (%s) from %s\ntimex: %s\nanchor: %s\n(\"%s\", \"%s\", \"%s\", \"%s\")",
          message, timex.text, timex.value, file, timex.elem, timex.anchor.map(_.elem),
          file.getName, timex.id, timex.text, timex.value)
        throw new Exception(exceptionMessage, cause)
      } else {
        error(message, timex, file)
      }
    }

    val normalizer = new TimeNormalizer

    val corpusStats = for (corpusFile <- options.getCorpusPaths.asScala) yield {
      val resultsIter: Iterator[Boolean] = for {
        file <- this.allFiles(corpusFile)
        doc = new TimeMLDocument(file)
        docCreationTime = TimeSpan.fromTimeMLValue(doc.creationTime.value)
        timex <- doc.timeExpressions
      } yield {
        val key = (file.getName, timex.id, timex.text, timex.value)
        val isAnnotationError = this.annotationErrors.contains(key)
        val isKnownFailure = this.knownFailures.contains(key)
        val isPossibleFailure = !isAnnotationError && !isKnownFailure

        // pick the single best parse and evaluate it
        val anchorOption = timex.anchor.flatMap(timex =>
          if (timex.value.isEmpty ||
            timex.value.startsWith("P") ||
            timex.value.startsWith("T") ||
            timex.value.contains('X')) None
          else Some(TimeSpan.fromTimeMLValue(timex.value)))
        val anchor = anchorOption.getOrElse(docCreationTime)

        // normalize the time expression given the anchor, and determine if it is correct
        val (value, isCorrect) =
          try {
            val temporal = normalizer.normalize(timex.text, anchor)
            val value = temporal.map(_.timeMLValue).getOrElse("")
            (value, value == timex.value)
          } catch {
            case e: Exception => ("", false)
          }
        
        // if a known error has been fixed, log it so that it can be removed from the list 
        if (isCorrect && isKnownFailure) {
          System.err.println("Failure has been fixed: " + key)
        }

        // if it's incorrect, log the error as informatively as possible
        if (!isCorrect && isPossibleFailure) {

          // collect all the possible parses, logging an error if there's a parsing error
          val possibleParses = 
            try {
              normalizer.parseAll(timex.text).toSeq 
            } catch {
              case e: UnsupportedOperationException =>
                fatal("Error parsing", timex, file, e)
                Seq.empty
            }

          // normalize each of the parses, collecting values and errors produced by the normalizer
          val possibleValueEithers = for (parse <- possibleParses) yield {
            try {
              Left(normalizer.normalize(parse, anchor).timeMLValue)
            } catch {
              case e @ (_: UnsupportedOperationException | _: DateTimeException) => Right(e)
            }
          }

          // log whether the normalizer produced all errors, all incorrect values, or just chose
          // the incorrect value out of the possible options
          val possibleValues = possibleValueEithers.collect{ case Left(value) => value }
          if (possibleValues.isEmpty) {
            for (Right(e) <- possibleValueEithers) fatal("Error parsing", timex, file, e)
          } else if (!possibleValues.toSet.contains(timex.value)) {
            fatal("All incorrect values %s for".format(possibleValues), timex, file, null)
          } else {
            error("Incorrect value %s chosen from %s for".format(value, possibleValues), timex, file)
          }
        }

        // yield whether the prediction was correct or not
        isCorrect
      }

      // calculate the total time expressions and the number of values that were correct
      val results = resultsIter.toSeq
      val total = results.size
      val correct = results.count(_ == true)
      (corpusFile, total, correct)
    }

    // print out performance on each corpus
    for ((corpusFile, total, correct) <- corpusStats) {
      printf("============================================================\n")
      printf("Corpus: %s\n", corpusFile)
      printf("Accuracy: %.3f\n", correct.toDouble / total.toDouble)
    }
    printf("============================================================\n")
  }

  def allFiles(fileOrDir: File): Iterator[File] = {
    if (fileOrDir.isDirectory) {
      fileOrDir.listFiles.iterator.map(this.allFiles).flatten
    } else {
      Iterator(fileOrDir)
    }
  }
}