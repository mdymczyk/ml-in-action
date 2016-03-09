package com.purogurammingu.classification.logisticregression

import org.scalatest.{Matchers, FunSuite}

/**
  * Created by mateusz on 3/1/16.
  */
class DatasetTest extends FunSuite with Matchers {

  test("Read default dataset") {
    val dataset = Dataset.load
    dataset.next() should be(DataPoint(Vector(1.0f, -0.017612f, 14.053064f),0))
  }

}
