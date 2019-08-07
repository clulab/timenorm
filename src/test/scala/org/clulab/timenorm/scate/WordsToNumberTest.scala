package org.clulab.timenorm.scate

import org.scalatest.FunSuite

class WordsToNumberTest extends FunSuite with TypesSuite {

  val enTextToNumber = WordsToNumber("en")

  test("English single token") {
    assert(enTextToNumber(Array("six")) === Some(6L))
    assert(enTextToNumber(Array("fifteen")) === Some(15L))
    assert(enTextToNumber(Array("thousand")) === Some(1000L))
    assert(enTextToNumber(Array("million")) === Some(1000000L))
    assert(enTextToNumber(Array("billion")) === Some(1000000000L))
    assert(enTextToNumber(Array("trillion")) === Some(1000000000000L))
  }

  test("English multi-token") {
    assert(enTextToNumber(Array("a", "hundred", "and", "one")) === Some(101L))
    assert(enTextToNumber(Array("six", "hundred", "five")) === Some(605L))
    assert(enTextToNumber(Array("eight", "thousand", "twenty", "two")) === Some(8022L))
    assert(enTextToNumber(Array("eight", "hundred", "and", "sixty", "nine")) === Some(869L))
    assert(enTextToNumber(Array("ninety", "thousand")) === Some(90000L))
    assert(enTextToNumber(Array("two", "hundred", "thousand", "four", "hundred", "eighty", "five")) === Some(200485L))
    assert(enTextToNumber(Array("seven", "million", "two")) === Some(7000002L))
    assert(enTextToNumber(Array("million", "six", "hundred", "eighty", "three", "thousand", "five", "hundred", "twenty", "two")) === Some(1683522L))
    assert(enTextToNumber(Array("seven", "hundred", "eighty", "million")) === Some(780000000L))
    assert(enTextToNumber(Array("four", "billion", "nineteen", "hundred")) === Some(4000001900L))
    assert(enTextToNumber(Array("ninety", "three", "trillion", "twenty", "million")) === Some(93000020000000L))
  }

  test("English multi-token variants") {
    assert(enTextToNumber(Array("nineteen", "eighteen")) === Some(1918L))
    assert(enTextToNumber(Array("twenty", "thirty", "eight")) === Some(2038L))
  }

  test("English invalid numbers") {
    assert(enTextToNumber(Array("several")) === None)
  }
}
