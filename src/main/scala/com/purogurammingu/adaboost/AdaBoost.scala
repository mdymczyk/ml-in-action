package com.purogurammingu.adaboost

import breeze.linalg.{DenseMatrix, DenseVector}
import com.purogurammingu.adaboost.AdaBoost.ThresholdInequality.ThresholdInequality

/**
  * Created by mateusz on 3/5/16.
  */
object AdaBoost {

  object ThresholdInequality extends Enumeration {
    type ThresholdInequality = Value
    val LT, GT = Value
  }

  def stumpClassify(dataMatrix: DenseMatrix[Double], dimen: Int, threshold: Double, thresholdIneq: ThresholdInequality) = {
    // TODO probably can get rid of this ugly var
    var retArray = DenseVector.ones[Double](dataMatrix.rows)
    thresholdIneq match {
      case ThresholdInequality.LT =>
        val array = retArray(dataMatrix(::, dimen) :<= threshold)
        // TODO can this be updated in a better way??
        array.keySet.foreach(i => array.update(i, -1.0))
      case ThresholdInequality.GT =>
        val array = retArray(dataMatrix(::, dimen) :> threshold)
        // TODO can this be updated in a better way??
        array.keySet.foreach(i => array.update(i, -1.0))
    }
    retArray
  }

}
