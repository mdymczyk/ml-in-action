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

    assertResult(Vector(-0.40546510810816444, -1.0986122886681098, -1.0986122886681098, -1.0986122886681098))(p0)
    assertResult(Vector(-0.8472978603872037, -1.252762968495368, -1.252762968495368, -1.252762968495368))(p1)
    assertResult(pAbsv)(0.6666666666666666)
  }

  test("Should classify documents") {
    val training = Dataset.loadVectorized
    val (p0, p1, pAbs) = NaiveBayes.train(training)
    val classified1 = NaiveBayes.classify(Dataset.words2Vec(Set("love", "my", "dalmation")), p0, p1,pAbs)
    assertResult(0)(classified1)
    val classified2 = NaiveBayes.classify(Dataset.words2Vec(Set("stupid", "garbage")), p0, p1,pAbs)
    assertResult(1)(classified2)

  }
}
