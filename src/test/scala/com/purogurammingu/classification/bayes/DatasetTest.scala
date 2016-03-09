package com.purogurammingu.classification.bayes

import com.purogurammingu.classification.nlp.Parser
import org.scalatest.FunSuite

import scala.io.Source
import scala.util.Random

/**
  * Created by mateusz on 2/12/16.
  */
class DatasetTest extends FunSuite {

  test("Should create vector out of words") {
    val vec = Dataset.words2Vec("my dog has flea problems help please".split(" ").toSeq)
    assert(vec == Vector(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

  test("Should generate probabilities for each word per class") {
    val (p0, p1, pAbsv) = NaiveBayes.train(Seq(
      PostingVector(Vector(1, 0, 1, 1), 1),
      PostingVector(Vector(1, 1, 0, 0), 1),
      PostingVector(Vector(1, 0, 0, 0), 0)
    ))

    assertResult(Vector(-0.40546510810816444, -1.0986122886681098, -1.0986122886681098, -1.0986122886681098))(p0)
    assertResult(Vector(-0.8472978603872037, -1.252762968495368, -1.252762968495368, -1.252762968495368))(p1)
    assertResult(pAbsv)(0.6666666666666666)
  }

  test("Should classify documents") {
    val training = Dataset.loadVectorized
    val (p0, p1, pAbs) = NaiveBayes.train(training)
    val classified1 = NaiveBayes.classify(Dataset.words2Vec(Seq("love", "my", "dalmation")), p0, p1, pAbs)
    assertResult(0)(classified1)
    val classified2 = NaiveBayes.classify(Dataset.words2Vec(Seq("stupid", "garbage")), p0, p1, pAbs)
    assertResult(1)(classified2)

  }

  test("Crossvalidate") {
    val training =
      (for (i <- 1 to 25) yield {
        List(
          new Posting(Parser.parse(readFile(s"/email/spam/$i.txt")), 1),
          new Posting(Parser.parse(readFile(s"/email/ham/$i.txt")), 0)
        )
      }).flatten
    val vocab = Dataset.vocabulary(training)

    val range = Random.shuffle(1 to 50).toList.take(10).toSet
    val test = training.zipWithIndex.filter {
      case (p, i) => range.contains(i)
    }.map {
      case (p, i) => new PostingVector(Dataset.words2Vec(vocab, p.text), p.abusive)
    }

    val (p0, p1, pAbs) =
      NaiveBayes.train(training.take(50).map(pos =>
        new PostingVector(Dataset.words2Vec(vocab, pos.text), pos.abusive)
      ))

    val errors = test.count { posVec => NaiveBayes.classify(posVec.words, p0, p1, pAbs) != posVec.abusive }
    println(s"Error rate ${errors.toFloat / test.size}")
  }

  private def readFile(file: String) = Source.fromFile(getClass.getResource(file).getPath).getLines().mkString
}
