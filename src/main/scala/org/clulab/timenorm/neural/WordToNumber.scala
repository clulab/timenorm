package org.clulab.timenorm.neural

object WordToNumber {

  private val Small: Map[String, Long] = Map(
    "zero" -> 0L,
    "one" -> 1L,
    "two" -> 2L,
    "three" -> 3L,
    "four" -> 4L,
    "five" -> 5L,
    "six" -> 6L,
    "seven" -> 7L,
    "eight" -> 8L,
    "nine" -> 9L,
    "ten" -> 10L,
    "eleven" -> 11L,
    "twelve" -> 12L,
    "thirteen" -> 13L,
    "fourteen" -> 14L,
    "fifteen" -> 15L,
    "sixteen" -> 16L,
    "seventeen" -> 17L,
    "eighteen" -> 18L,
    "nineteen" -> 19L,
    "twenty" -> 20L,
    "thirty" -> 30L,
    "forty" -> 40L,
    "fifty" -> 50L,
    "sixty" -> 60L,
    "seventy" -> 70L,
    "eighty" -> 80L,
    "ninety" -> 90L
  )

  private val Magnitude: Map[String, Long] = Map(
    "thousand" -> 1000L,
    "million" -> 1000000L,
    "billion" -> 1000000000L,
    "trillion" -> 1000000000000L,
    "quadrillion" -> 1000000000000000L,
    "quintillion" -> 1000000000000000000L
    // "sextillion" ->    1000000000000000000000L,
    // "septillion" -> 1000000000000000000000000L,
    // "octillion" -> 1000000000000000000000000000L,
    // "nonillion" ->    1000000000000000000000000000000L,
    // "decillion" ->    1000000000000000000000000000000000L
  )

  def convert(string: String): String = {
    try {
      var n = 0L
      var g = 0L
      for (w <- string.split("[\\s-]+")) {
        try {
          g += Small(w)
        } catch {
          case e: NoSuchElementException =>
            if (w == "hundred" && g!=0)
              g *= 100L
            else {
              n += g * Magnitude(w)
              g = 0L
            }
          }
      }
      (n + g).toString
    } catch {
      case e: Exception => string
    }
  }
}
