package info.bethard.timenorm.formal

import info.bethard.anafora.Data

object ParseAnaforaXMLs {
  def main(args: Array[String]): Unit = {
    for (path <- args) {
      println(path)
      implicit val data = Data.fromPath(path)
      for (entity <- data.entities) {
        println(AnaforaReader.temporal(entity))
      }
    }
  }

}
