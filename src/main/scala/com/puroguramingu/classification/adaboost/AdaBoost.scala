package com.puroguramingu.classification.adaboost

import breeze.linalg._
import com.puroguramingu.classification.adaboost.AdaBoost.ThresholdInequality.ThresholdInequality

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
    val m, n = dataMatrix.cols, dataMatrix.rows
    val steps = 10
    val bestStump = mutable.Map[String, Any]()
    var bestClassEst = DenseVector.zeros[Double](m)
    var minError = Double.PositiveInfinity

    for (i <- 0 until n) {
      val rangeMin = min(dataMatrix(::, i))
      val rangeMax = max(dataMatrix(::, i))
      val stepSize = (rangeMax - rangeMin) / steps
      for (j <- -1 to steps) {
        for (inequal <- ThresholdInequality.values) {
          val threshVal = (rangeMin + j) * stepSize
          val predictedVals = stumpClassify(dataMatrix, i, threshVal, inequal)
          val errArr = DenseVector.ones[Double](m)
          val array = predictedVals :== labels
          // TODO can this be updated in a better way??
          array.keySet.foreach(i => errArr.update(i, 0))
          val weightedError = D.t * errArr
          if (weightedError < minError) {
            minError = weightedError
            bestClassEst = predictedVals.copy
            bestStump.put("dim", i)
            bestStump.put("thresh", threshVal)
            bestStump.put("ineq", inequal)
          }
        }
      }
    }
    (bestStump, minError, bestClassEst)
  }

  def adaBoostTrainDS(dataMatrix: DenseMatrix[Double], labels: DenseVector[Double], numIt: Int = 40) = {
    var weakClassArr = mutable.Buffer[mutable.Map[String, Any]]()
    val m = dataMatrix.cols
    var D: DenseVector[Double] = DenseVector.ones[Double](m) / m.toDouble
    val aggClassEst = DenseVector.zeros[Double](m)
    for (i <- 0 until numIt) {
      val (bestStump, error, classEst) = buildStump(dataMatrix, labels, D)
      val alpha = 0.5 * math.log((1.0 - error) / max(error, 1e-16))
      bestStump.put("alpha", alpha)
      weakClassArr.+=(bestStump)
      val expon = (-1 * alpha * labels.t) * classEst
      D = D * expon.map(math.exp)
      D = D/sum(D)
      aggClassEst += alpha*classEst
      // TODO finish (ADA BOOST chapter)
    }
  }

}
