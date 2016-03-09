package com.purogurammingu.classification.nlp

import org.scalatest.{Matchers, FunSuite}

/**
  * Created by mateusz on 2/28/16.
  */
class ParserTest extends FunSuite with Matchers {

  test("Should return parsed tokens.") {
    val parsed = Parser.parse("This book is the best book on Python or M.L. I have ever laid eyes upon.")
    parsed should not be null
    parsed should have size 11
    parsed should contain theSameElementsAs List("this", "book", "the", "best", "book", "python", "have", "ever", "laid", "eyes", "upon")
  }

}
