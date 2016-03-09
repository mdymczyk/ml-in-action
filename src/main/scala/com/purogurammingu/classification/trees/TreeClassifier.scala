package com.purogurammingu.classification.trees

import com.purogurammingu.classification.dataset.Dataset

/**
  * Created by mateusz on 2/6/16.
  */
class TreeClassifier[T](training: Dataset[T]) {

  val root = new TreeNode(training)

  def classify(event: Vector[Double]) = root.classify(event)

}

class TreeNode[T](training: Dataset[T]) {

  def classify(event: Vector[Double]): T = leaf match {
    case true => label.get
    case false => children.get.get(event(feature.get)).get.classify(Dataset.remove(event, feature.get))
  }

  val leaf = training.uniqueClasses == 1
  val label = if (leaf) Some(training.uniqueClass) else None
  val feature = if (leaf) None else Some(training.bestSplit._1)
  val children = if (leaf) None else Some(training.bestSplit._2.map { case ((key, value)) => (key, new TreeNode[T](value)) })

}