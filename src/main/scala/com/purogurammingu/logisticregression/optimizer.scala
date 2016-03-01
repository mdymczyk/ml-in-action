package com.purogurammingu.logisticregression

/**
  * Created by mateusz on 3/1/16.
  */
object optimizer {

  def sigmoid(z: Double) = 1.0f / (1 + math.exp(-z) )

  def gradientAscent(data: Seq[DataPoint]) = {
    
  }

}
