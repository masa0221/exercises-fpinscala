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
      intAddition.op(2, 3) should equal(6)
    }
  }
  "booleanOr" - {
    "unimplements" in {
      true should equal(true)
    }
  }
  "booleanAnd" - {
    "unimplements" in {
      true should equal(true)
    }
  }
