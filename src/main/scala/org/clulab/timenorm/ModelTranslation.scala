package org.clulab.timenorm.neural

import org.deeplearning4j.nn.conf.inputs.InputType
import org.deeplearning4j.nn.conf.layers.samediff.SameDiffLambdaLayer
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.modelimport.keras.{KerasLayer, KerasModelImport}
import org.deeplearning4j.util.ModelSerializer
import org.nd4j.autodiff.samediff.{SDVariable, SameDiff}


class ReverseLambdaDim1 extends SameDiffLambdaLayer {
  override def defineLayer(sd: SameDiff, x: SDVariable): SDVariable = sd.reverse("reversed", x, 1)
  override def getOutputType(layerIndex: Int, inputType: InputType): InputType = inputType
}

class ReverseLambdaDim0 extends SameDiffLambdaLayer {
  override def defineLayer(sd: SameDiff, x: SDVariable): SDVariable = sd.reverse("reversed", x, 0)
  override def getOutputType(layerIndex: Int, inputType: InputType): InputType = inputType
}

object ModelTranslation {

  def main(args: Array[String]): Unit = {
    KerasLayer.registerLambdaLayer("lambda_1", new ReverseLambdaDim1())
    KerasLayer.registerLambdaLayer("lambda_2", new ReverseLambdaDim0())
    val model: ComputationGraph = KerasModelImport.importKerasModelAndWeights(args(0), false)
    ModelSerializer.writeModel(model, args(1), false)
  }
}
