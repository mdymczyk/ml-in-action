package com.puroguramingu.classification.adaboost

import breeze.linalg.{DenseVector, DenseMatrix}

/**
  * Created by mateusz on 3/5/16.
  */
object Dataset {

  def load = (DenseMatrix( Seq(
   Vector(1.0, 2.1),
   Vector(2.0, 1.1),
   Vector(1.3, 1.0),
   Vector(1.0, 1.0),
   Vector(2.0, 1.0)
  ):_*), DenseVector(1.0, 1.0, -1.0, -1.0, -1.0))

}
