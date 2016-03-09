package com.purogurammingu.classification.dataset

/**
  * Created by mateusz on 2/6/16.
  */
case class Dataset[T](
  observations: Seq[Observation[T]]
) {

  def uniqueClass = observations.groupBy(_.label).head._1

  def uniqueClasses = observations.groupBy(_.label).size

  def classProbabilities = {
    observations.groupBy(_.label).mapValues(_.size.toDouble / observations.size)
  }

  val entropy = {
    classProbabilities.foldRight(0.0) { case ((label, prob), acc) =>
      acc - prob * (Math.log(prob) / Math.log(2))
    }
  }

  def slice(feature: Int, value: Double): Dataset[T] = {
    new Dataset(
      observations.
        filter(_.features(feature) == value).
        map(observation =>
          new Observation[T](Dataset.remove(observation.features, feature), observation.label)
        )
    )
  }

  def bestSplit: (Int, Map[Double, Dataset[T]]) = {
    val bestFeature = bestSplitFeature
    val featureVals = featureValues(bestFeature)
    (bestFeature, featureVals.map(value => (value, slice(bestFeature, value))).toMap)
  }

  def bestSplitFeature = {
    features.foldRight((0.0, -1)) { (feature, currentBest) =>
      val newEntropy = this.featureValues(feature).foldRight(0.0) { case (featureVal, acc) =>
        val slice = this.slice(feature, featureVal)
        acc + slice.entropy * (slice.observations.length / observations.length.toDouble)
      }
      val informationGain = entropy - newEntropy
      if (informationGain > currentBest._1) {
        (informationGain, feature)
      } else {
        currentBest
      }
    }._2
  }

  private def features = observations.head.features.indices

  private def featureValues(feature: Int): Set[Double] = observations.map(_.features(feature)).distinct.toSet

}

object Dataset {
  def remove(vec: Vector[Double], id: Int) = {
    (vec take id) ++ (vec drop (id + 1))
  }
}

case class Observation[T](
  features: Vector[Double],
  label: T
)
