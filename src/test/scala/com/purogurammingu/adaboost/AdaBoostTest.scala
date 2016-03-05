package com.purogurammingu.adaboost

import breeze.linalg.DenseVector
import com.purogurammingu.adaboost.AdaBoost.ThresholdInequality
import org.scalatest.{Matchers, FunSuite}

/**
  * Created by mateusz on 3/5/16.
  */
class AdaBoostTest extends FunSuite with Matchers {

  test("Stump LT") {
    val res = AdaBoost.stumpClassify(Dataset.load._1, 0, 1.5, ThresholdInequality.LT)
    res should be (new DenseVector(Array(-1.0, 1.0, -1.0, -1.0, 1.0)))
  }

  test("Stump GT") {
    val res = AdaBoost.stumpClassify(Dataset.load._1, 0, 1.5, ThresholdInequality.GT)
    res should be (new DenseVector(Array(1.0, -1.0, 1.0, 1.0, -1.0)))
  }

}
