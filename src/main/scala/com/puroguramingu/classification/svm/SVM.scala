package com.puroguramingu.classification.svm

import breeze.linalg.{DenseVector, DenseMatrix}

/**
  * Created by mateusz on 3/4/16.
  */
object SVM {

  def simpleSMO(dataset: Seq[LabeledPoint], C: Int, tol: Int, maxIter: Int) = {
    val data: DenseMatrix[Double] = DenseMatrix(dataset.map(_.pt.toArray): _*)
    val labels = DenseVector(dataset.map(_.label): _*)
    var b = 0
    val (m,n) = (data.rows, data.cols)
    for(i <- 0 to maxIter) {
      // TODO implement, maybe one day :-)
    }
  }

}
