package com.puroguramingu.valueprediction.regression

import breeze.linalg.{inv, det, DenseVector, DenseMatrix}

import scala.io.Source

/**
  * Created by mateusz on 3/9/16.
  */
object Regression {

  def loadData(file: String) = {
    val x: Iterator[(Array[Double], Double)] = Source.fromFile(file)
      .getLines()
      .map( _.split("\\t").map(_.toDouble) )
      .map( x => (x.take(x.length - 1), x.last) )
    (DenseMatrix(x.map(_._1).toSeq:_*), DenseVector(x.map(_._2).toArray:_*))
  }

  def standardRegression(data: DenseMatrix[Double], labels: DenseVector[Double]) = {
    // TODO this shouldnt be an element wise operation but a matrix by matrix multiplication...
    val xTx = data.t :* data
    det(xTx) match {
      // TODO will this work!? what about rounding errors?
      case 0.0 => throw new IllegalArgumentException("Singular matrix, cannot be inversed!")
      case _ => inv(xTx) * (data.t * labels)
    }

  }

}
