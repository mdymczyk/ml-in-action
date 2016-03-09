package com.puroguramingu.classification.svm

import org.scalatest.{Matchers, FunSuite}

/**
  * Created by mateusz on 3/4/16.
  */
class DatasetTest extends FunSuite with Matchers {

  test("Randomize a different number") {
    (1 to 1000).foreach { i =>
      val x = Dataset.selectJrand(1, 3)
      x should not be 1
    }
  }

}
