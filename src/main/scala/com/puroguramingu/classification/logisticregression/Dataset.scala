package com.puroguramingu.classification.logisticregression

import scala.io.Source

/**
  * Created by mateusz on 3/1/16.
  */
object Dataset {

  def load: Iterator[DataPoint] = load("/dataset/logisticregression/testSet.txt")

  def load(src: String): Iterator[DataPoint] =
    Source.fromFile(getClass.getResource(src).getPath).getLines().map(_.stripMargin.split("\\s")).map { arr =>
      DataPoint(Vector(1.0f, arr(0).toFloat, arr(1).toFloat), arr(2).toInt)
    }
}

case class DataPoint(
  features: Vector[Float],
  label: Int
)