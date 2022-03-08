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
      expected should equal(384748)
    }
  }

  "double" - {
    "0から1未満までの値になること" in {
      val (expected, rng) = RNG.double(RNG.Simple(1))
      expected should (be >= 0.0 and be < 1.0)
    }
  }

  "intDouble" - {
    "int と double のタプルが取得できること" in {
      val ((expectedInt, expectedDouble), rng) = RNG.intDouble(RNG.Simple(1))
      expectedInt should equal(384748)
      expectedDouble should (be >= 0.0 and be < 1.0)
    }
  }

  "doubleInt" - {
    "double とint のタプルが取得できること" in {
      val ((expectedDouble, expectedInt), rng) = RNG.doubleInt(RNG.Simple(1))
      expectedDouble should (be >= 0.0 and be < 1.0)
      expectedInt should equal(384748)
    }
  }

  "double3" - {
    "double 3つのタプルが取得できること" in {
      val ((d1, d2, d3), rng) = RNG.double3(RNG.Simple(1))
      d1 should (be >= 0.0 and be < 1.0)
      d2 should (be >= 0.0 and be < 1.0)
      d3 should (be >= 0.0 and be < 1.0)
    }
  }

  "ints" - {
    "ランダムな値のリストを取得できること" in {
      val (expected, rng) = RNG.ints(5)(RNG.Simple(1))
      expected.length should equal(5)
      expected should equal(
        List(384748, 1151252338, 549383846, 1612966641, 883454041)
      )
    }
  }

  "doubleViaMap" - {
    "0から1未満までの値になること" in {
      val (expected, rng) = RNG.doubleViaMap(RNG.Simple(1))
      expected should (be >= 0.0 and be < 1.0)
    }
  }

  "map2" - {
    "二つのRandの結果に対して関数を適用できること" in {
      val (expected, rng) =
        RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)(_ + _)(RNG.Simple(1))
      // 1151637086 = 384748 + 1151252338
      expected should equal(1151637086)
    }
  }

  "sequence" - {
    "指定した関数のリストの結果がリストになって取得できること" in {
      val rs = List(
        RNG.nonNegativeInt,
        RNG.nonNegativeInt,
        RNG.nonNegativeInt,
        RNG.nonNegativeInt,
        RNG.nonNegativeInt
      )
      val (expected, rng2) = RNG.sequence(rs)(RNG.Simple(1))
      expected should equal(
        List(384748, 1151252338, 549383846, 1612966641, 883454041)
      )
    }
  }

  "intsViaSequence" - {
    "ランダムな値のリストを取得できること" in {
      val (expected, rng) = RNG.intsViaSequence(5)(RNG.Simple(1))
      expected.length should equal(5)
      expected should equal(
        List(384748, 1151252338, 549383846, 1612966641, 883454041)
      )
    }
  }

  "flatMap" - {
    "flatMapの実装が動くこと" in {
      val (expected, rng) =
        RNG.flatMap(RNG.nonNegativeInt)(a => RNG.unit(a + 1))(RNG.Simple(1))
      expected should equal(384749)
    }
  }

  "nonNegativeLessThan" - {
    val (expected, rng) = RNG.nonNegativeLessThan(2)(RNG.Simple(1))
    expected should equal(1)
  }
end StateTest
