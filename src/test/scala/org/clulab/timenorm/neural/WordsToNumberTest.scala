package org.clulab.timenorm.neural

import org.clulab.timenorm.formal.TypesSuite
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class WordsToNumberTest extends FunSuite with TypesSuite {

  val textToNumber = WordsToNumber("en")

  test("English single token") {
    assert(textToNumber(Array("six")) === 6L)
    assert(textToNumber(Array("fifteen")) === 15L)
    assert(textToNumber(Array("thousand")) === 1000L)
    assert(textToNumber(Array("million")) === 1000000L)
    assert(textToNumber(Array("billion")) === 1000000000L)
    assert(textToNumber(Array("trillion")) === 1000000000000L)
  }

  test("English multi-token") {
    assert(textToNumber(Array("a", "hundred", "and", "one")) === 101L)
    assert(textToNumber(Array("six", "hundred", "five")) === 605L)
    assert(textToNumber(Array("eight", "thousand", "twenty", "two")) === 8022L)
    assert(textToNumber(Array("eight", "hundred", "and", "sixty", "nine")) === 869L)
    assert(textToNumber(Array("ninety", "thousand")) === 90000L)
    assert(textToNumber(Array("two", "hundred", "thousand", "four", "hundred", "eighty", "five")) === 200485L)
    assert(textToNumber(Array("seven", "million", "two")) === 7000002L)
    assert(textToNumber(Array("million", "six", "hundred", "eighty", "three", "thousand", "five", "hundred", "twenty", "two")) === 1683522L)
    assert(textToNumber(Array("seven", "hundred", "eighty", "million")) === 780000000L)
    assert(textToNumber(Array("four", "billion", "nineteen", "hundred")) === 4000001900L)
    assert(textToNumber(Array("ninety", "three", "trillion", "twenty", "million")) === 93000020000000L)
  }

  test("English multi-token variants") {
    assert(textToNumber(Array("nineteen", "eighteen")) === 1918L)
    assert(textToNumber(Array("twenty", "thirty", "eight")) === 2038L)
  }
}
