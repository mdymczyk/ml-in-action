package com.puroguramingu.classification.dataset

import org.scalatest.FunSuite

/**
  * Created by mateusz on 2/6/16.
  */
class DatasetTest extends FunSuite {

  test("Should return proper entropy") {
    assert(new Dataset(
      Seq(
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 0), "n"),
        new Observation(Vector(0, 1), "n"),
        new Observation(Vector(0, 1), "n")
      )
    ).entropy == 0.9709505944546686)
  }

  test("Entropy should grow when the dataset gets more mixed") {
    val twoClasses = new Dataset(
      Seq(
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 0), "n"),
        new Observation(Vector(0, 1), "n")
      )
    ).entropy
    val threeClasses = new Dataset(
      Seq(
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 0), "n"),
        new Observation(Vector(0, 1), "m")
      )
    ).entropy
    assert(threeClasses > twoClasses)
  }

  test("Should return a slice of the dataset") {
    val dataset = new Dataset(
      Seq(
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 0), "n"),
        new Observation(Vector(0, 1), "n"),
        new Observation(Vector(0, 1), "n")
      )
    )
    assert(dataset.slice(0,1).observations.size == 3)
    assert(dataset.slice(0,0).observations.size == 2)
  }

  test("Should choose the feature with the best information gain") {
    assert(new Dataset(
      Seq(
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 0), "n"),
        new Observation(Vector(0, 1), "n"),
        new Observation(Vector(0, 1), "n")
      )
    ).bestSplitFeature == 0)
  }

}
