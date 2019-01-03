package org.clulab.timenorm.neural

import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.deeplearning4j.util.ModelSerializer


object ModelTranslation {

  def main(args: Array[String]): Unit = {
    val model: ComputationGraph = KerasModelImport.importKerasModelAndWeights(args(0), false)
    ModelSerializer.writeModel(model, args(1), false)
  }
}
