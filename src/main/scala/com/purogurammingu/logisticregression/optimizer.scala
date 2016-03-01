package com.purogurammingu.logisticregression

import breeze.linalg.{Vector, DenseMatrix, DenseVector}

/**
  * Created by mateusz on 3/1/16.
  */
object optimizer {

  def sigmoid(z: Double) = 1.0f / (1 + math.exp(-z) )

  def gradientAscent(dataSet: Seq[DataPoint]): Vector[Double] = {
    val data = DenseMatrix(dataSet.map(_.features.map(_.toDouble).toArray) :_*)
    val labels = DenseVector(dataSet.map(_.label.toDouble).toArray :_*)
    val alpha = 0.001
    val maxIter = 100
    var weights = DenseVector.ones[Double](dataSet.head.features.length)
    for(i <- 1 to maxIter) {
      val h = data * weights
      val error = labels - h
      weights = weights + alpha * (data.t * error)
    }
    weights.toVector
  }

}
