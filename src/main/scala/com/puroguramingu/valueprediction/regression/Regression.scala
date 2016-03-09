package com.puroguramingu.valueprediction.regression

import breeze.linalg.{DenseVector, DenseMatrix}

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

}
