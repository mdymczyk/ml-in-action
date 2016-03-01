package com.purogurammingu.logisticregression

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
    res should be(DenseVector(2.1783800570102704E73, 5.675074139053176E72, 2.130471894278055E74))
  }

}
