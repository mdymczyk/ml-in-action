package com.puroguramingu.classification.trees

import com.puroguramingu.classification.dataset.{Observation, Dataset}
import org.scalatest.FunSuite

/**
  * Created by mateusz on 2/11/16.
  */
class TreeClassifierTest extends FunSuite {

  test("Should classify") {
    val ds = new Dataset(
      Seq(
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 1), "y"),
        new Observation(Vector(1, 0), "n"),
        new Observation(Vector(0, 1), "n"),
        new Observation(Vector(0, 1), "n")
      )
    )
    val classifier = new TreeClassifier(ds)
    assert(classifier.classify(Vector(1, 1)) == "y")
    assert(classifier.classify(Vector(0, 1)) == "n")
    assert(classifier.classify(Vector(0, 0)) == "n")
  }

}
