package com.purogurammingu.classification.nlp

/**
  * Created by mateusz on 2/28/16.
  */
object Parser {

  val splitter = "\\w*".r

  def parse(text: String) = splitter.findAllMatchIn(text).map(_.matched.toLowerCase).filter(_.length > 2).toList
}
