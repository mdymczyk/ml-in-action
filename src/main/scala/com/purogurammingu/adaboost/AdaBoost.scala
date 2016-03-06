package com.purogurammingu.adaboost

import breeze.linalg.{max, min, DenseMatrix, DenseVector}
import com.purogurammingu.adaboost.AdaBoost.ThresholdInequality.ThresholdInequality

import scala.collection.mutable

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

  def buildStump(dataMatrix: DenseMatrix[Double], labels: DenseVector[Double], D: DenseVector[Double]) = {
    val m,n = dataMatrix.cols, dataMatrix.rows
    val steps = 10
    val bestStump = mutable.Map[String, Any]()
    var bestClassEst = DenseVector.zeros[Double](m)
    var minError = Double.PositiveInfinity

    for(i <- 0 until n) {
      val rangeMin = min(dataMatrix(::,i))
      val rangeMax = max(dataMatrix(::,i))
      val stepSize = (rangeMax - rangeMin) / steps
      for(j <- -1 to steps) {
        for(inequal <- ThresholdInequality.values) {
          val threshVal = (rangeMin + j) * stepSize
          val predictedVals = stumpClassify(dataMatrix, i, threshVal, inequal)
          val errArr = DenseVector.ones[Double](m)
          val array = predictedVals :== labels
          // TODO can this be updated in a better way??
          array.keySet.foreach(i => errArr.update(i, 0))
          val weightedError = D.t*errArr
          if (weightedError < minError) {
            minError = weightedError
            bestClassEst = predictedVals.copy
            bestStump.put("dim", i)
            bestStump.put("thresh", threshVal)
            bestStump.put("ineq", inequal)
          }
        }
      }
      (bestStump, minError, bestClassEst)
    }
  }

}
