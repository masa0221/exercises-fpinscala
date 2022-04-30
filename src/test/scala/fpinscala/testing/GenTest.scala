package fpinscala.testing

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import fpinscala.state.RNG

class GenTest extends AnyFreeSpecLike with Matchers:
  "choose" - {
    "指定した最大値と最小値の間の値が生成できること" in {
      val min = 10
      val max = 20
      (1 to 10000).foreach { i =>
        val (expected, rng) = Gen.choose(min, max).sample.run(RNG.Simple(i))
        expected should (be >= min and be <= max)
      }
    }
  }

  "listOfN" - {
    "指定した件数のリストが生成できること" in {
      val min = 10
      val max = 20
      val count = 10
      val (expected, rng) =
        Gen.listOfN(count, Gen.choose(min, max)).sample.run(RNG.Simple(1))
      expected.length should equal(count)
      all(expected) should (be >= min and be <= max)
    }
  }

  "flatMap" - {
    "flatMapできること" in {
      val (expected, rng) =
        Gen
          .unit(() => 1)
          .flatMap(a => Gen.unit(() => a() + 1))
          .sample
          .run(RNG.Simple(1))
      expected() should equal(2)
    }
  }
