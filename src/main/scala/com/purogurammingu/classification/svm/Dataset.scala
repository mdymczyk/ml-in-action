package com.purogurammingu.classification.svm

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

/**
  * Created by mateusz on 3/4/16.
  */
object Dataset {

  def load(file: String): Iterator[Array[LabeledPoint]] = {
    Source.fromFile(getClass.getResource(file).getPath).getLines().map { line =>
      line.stripLineEnd.split("\t").map { data =>
        LabeledPoint(Vector(data(0).toDouble, data(1).toDouble), data(2))
      }
    }
  }

  @tailrec
  def selectJrand(i: Int, m: Int): Int = {
    Random.nextInt(m) match {
      case x if x == i => selectJrand(i, m)
      case x => x
    }
  }

  def clipAlpha(aj: Int, H: Int, L: Int): Int = Math.max(Math.min(aj, H), L)

}

case class LabeledPoint(val pt: Vector[Double], val label: Int)