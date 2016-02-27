package com.purogurammingu.bayes

import org.scalatest.FunSuite

/**
  * Created by mateusz on 2/12/16.
  */
class DatasetTest extends FunSuite {

  test("Should create vector out of words") {
    val vec = Dataset.words2Vec("my dog has flea problems help please".split(" ").toSet)
    assert(vec == Vector(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

  test("Should generate probabilities for each word per class") {
    val (p0, p1, pAbsv) = NaiveBayes.train(Seq(
      PostingVector(Vector(1,0,1,1), 1),
      PostingVector(Vector(1,1,0,0), 1),
      PostingVector(Vector(1,0,0,0), 0)
    ))

    assertResult(p0)(Vector(1,0,0,0))
    assertResult(p1)(Vector(0.4,0.2,0.2,0.2))
    assertResult(pAbsv)(0.6666666666666666)
  }

}