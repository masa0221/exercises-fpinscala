package fpinscala.state

import fpinscala.state.RNG.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class StateTest extends AnyFreeSpecLike with Matchers:
  "nonNegativeInt" - {
    "同じ値になること" in {
      // (何度テストを実行しても同じになることで担保する)
      // scala> fpinscala.state.RNG.Simple(1).nextInt
      // val res0: (Int, fpinscala.state.RNG) = (384748,Simple(25214903928))
      val (expected, rng) = RNG.nonNegativeInt(RNG.Simple(1))
      expected `shouldBe` 384748
    }
  }
end StateTest
