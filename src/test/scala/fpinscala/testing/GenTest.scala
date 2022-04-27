package fpinscala.testing

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import fpinscala.state.RNG

class GenTest extends AnyFreeSpecLike with Matchers:
  // case class Gen[A](sample: State[RNG, A])
  // def choose(start: Int, stopExclusive: Int): Gen[Int] =
  "choose" - {
    "指定した最大値と最小値の間の値が生成できること" in {
      val min = 10
      val max = 20
      (1 to 1000).foreach { i =>
        val (expected, rng) = Gen.choose(min, max).sample.run(RNG.Simple(i))
        expected should (be >= min and be <= max)
      }
    }
  }
