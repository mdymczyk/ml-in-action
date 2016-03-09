package com.purogurammingu.classification.logisticregression

import breeze.linalg.DenseVector
import org.scalatest.{Matchers, FunSuite}

/**
  * Created by mateusz on 3/1/16.
  */
class OptimizerTest extends FunSuite with Matchers {

  test("Correct sigmoid function") {
    optimizer.sigmoid(0) should be(0.5)
  }

  test("Gradient ascent") {
    val res = optimizer.gradientAscent(Dataset.load.toSeq)
    res should be(DenseVector(2.0622499639928726, 0.3506131194460708, -0.36384860759363286))
  }

  test("Stochastic gradient ascent") {
    val res = optimizer.stochasticGradientAscent0(Dataset.load.toSeq)
    res should be(DenseVector(0.9639510048159127, 0.9826865994733246, 0.49153885858722196))
  }

  test("Improved stochastic gradient ascent") {
    val res = optimizer.stochasticGradientAscent(Dataset.load.toSeq)
    res should be(DenseVector(13.623722900658205, 1.1613102976486815, -1.809595008405913))
  }
}
