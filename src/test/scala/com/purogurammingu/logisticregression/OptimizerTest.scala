package com.purogurammingu.logisticregression

import org.scalatest.{Matchers, FunSuite}

/**
  * Created by mateusz on 3/1/16.
  */
class OptimizerTest extends FunSuite with Matchers {

  test("Correct sigmoid function") {
    optimizer.sigmoid(0) should be(0.5)
  }

}
