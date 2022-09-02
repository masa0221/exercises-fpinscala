package fpinscala.monoids

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class MonoidTest extends AnyFreeSpecLike with Matchers:
  "stringMonad" - {
    "足し算ができること" in {
      stringMonad.op("abcd", "efg") should equal("abcdefg")
    }
  }
  "intAddition" - {
    "足し算ができること" in {
      intAddition.op(2, 3) should equal(5)
    }
  }
  "intMultiplication" - {
    "掛け算ができること" in {
      intMultiplication.op(2, 3) should equal(6)
    }
  }
  "booleanOr" in {
    booleanOr.op(false, false) should equal(false)
    booleanOr.op(false, true) should equal(true)
    booleanOr.op(true, false) should equal(true)
    booleanOr.op(true, true) should equal(true)
  }
  "booleanAnd" in {
    booleanAnd.op(false, false) should equal(false)
    booleanAnd.op(false, true) should equal(false)
    booleanAnd.op(true, false) should equal(false)
    booleanAnd.op(true, true) should equal(true)
  }
