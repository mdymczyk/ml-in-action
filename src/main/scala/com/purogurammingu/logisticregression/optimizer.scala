package com.purogurammingu.logisticregression

import breeze.linalg._

import scala.util.Random

/**
  * Created by mateusz on 3/1/16.
  */
object optimizer {

  def sigmoid(z: Double) = 1.0f / (1 + math.exp(-z))

  def gradientAscent(dataSet: Seq[DataPoint]): Vector[Double] = {
    val data = DenseMatrix(dataSet.map(_.features.map(_.toDouble).toArray): _*)
    val labels = DenseVector(dataSet.map(_.label.toDouble).toArray: _*)
    val alpha = 0.001
    val maxIter = 100
    var weights = DenseVector.ones[Double](dataSet.head.features.length)
    for (i <- 1 to maxIter) {
      val h = (data * weights).map(sigmoid)
      val error = labels - h
      weights = weights + alpha * (data.t * error)
    }
    weights.toVector
  }

  def stochasticGradientAscent(dataSet: Seq[DataPoint], maxIter: Int = 150): Vector[Double] = {
    val data = DenseMatrix(dataSet.map(_.features.map(_.toDouble).toArray): _*)
    val labels = DenseVector(dataSet.map(_.label.toDouble).toArray: _*)
    val n = dataSet.head.features.length
    val m = dataSet.size
    var weights = DenseVector.ones[Double](n)

    for (j <- 0 until maxIter) {
      val range = Random.shuffle( (0 until m).toList )
      for (i <- 0 until m) {
        val alpha = 4/(1.0+j+i)+0.01
        val row = data(range(i), ::).t
        val h = sigmoid(sum(row :* weights))
        val error = labels(range(i)) - h
        weights = weights + alpha * (row * error)
      }
    }
    weights
  }

  def stochasticGradientAscent0(dataSet: Seq[DataPoint]): Vector[Double] = {
    val data = DenseMatrix(dataSet.map(_.features.map(_.toDouble).toArray): _*)
    val labels = DenseVector(dataSet.map(_.label.toDouble).toArray: _*)
    val alpha = 0.001
    val n = dataSet.head.features.length
    val m = dataSet.size
    var weights = DenseVector.ones[Double](n)
    for (i <- 0 until m) {
      val row = data(i,::).t
      val h = sigmoid(sum(row :* weights))
      val error = labels(i) - h
      weights = weights + alpha * (row * error)
    }
    weights
  }

}
