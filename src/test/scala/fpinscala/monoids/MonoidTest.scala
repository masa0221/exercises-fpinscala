package fpinscala.monoids

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class MonoidTest extends AnyFreeSpecLike with Matchers:
  "stringMonad" - {
    "足し算ができること" in {
      stringMonad.op("abcd", "efg") should equal("abcdefg")
    }
  }
  "intAddition" - {}
  "intMultiplication" - {}
  "booleanOr" - {}
  "booleanAnd" - {}
