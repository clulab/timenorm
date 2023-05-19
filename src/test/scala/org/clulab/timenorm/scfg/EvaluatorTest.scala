package org.clulab.timenorm.scfg

import org.scalatest.FunSuite

class EvaluatorTest extends FunSuite {

  test("runs without exception") {
    Evaluator.main("es", "./datasets/es/test.tsv", "../es-test.tsv")
  }
}
