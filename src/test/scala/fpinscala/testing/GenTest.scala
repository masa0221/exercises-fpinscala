package fpinscala.testing

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import fpinscala.state.RNG
import org.scalactic.Pass

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
        Gen.choose(min, max).listOfN(Gen.unit(count)).sample.run(RNG.Simple(1))
      expected.length should equal(count)
      all(expected) should (be >= min and be <= max)
    }
  }

  "flatMap" - {
    "flatMapできること" in {
      val (expected, rng) =
        Gen
          .unit(1)
          .flatMap(a => Gen.unit(a + 1))
          .sample
          .run(RNG.Simple(1))
      expected should equal(2)
    }
  }

  "union" - {
    "1つ目のbooleanがfalseの場合は1つ目の値になること" in {
      // scala> val (v, rng) = fpinscala.state.RNG.Simple(1).nextInt
      // val v: Int = 384748
      // val rng: fpinscala.state.RNG = Simple(25214903928)

      // scala> val (v2, rng2) = rng.nextInt
      // val v2: Int = -1151252339
      // val rng2: fpinscala.state.RNG = Simple(206026503483683)

      // scala> val (v3, rng3) = rng2.nextInt
      // val v3: Int = -549383847
      // val rng3: fpinscala.state.RNG = Simple(245470556921330)
      val g1 = Gen.unit(1)
      val g2 = Gen.unit(2)
      val (expected, rng) =
        Gen
          .union(g1, g2)
          .sample
          .run(RNG.Simple(25214903928L))
      expected should equal(1)
    }
    "1つ目のbooleanがtrueの場合は2つ目の値になること" in {
      // scala> val (v, rng) = fpinscala.state.RNG.Simple(1).nextInt
      // val v: Int = 384748
      // val rng: fpinscala.state.RNG = Simple(25214903928)
      val g1 = Gen.unit(1)
      val g2 = Gen.unit(2)
      val (expected, rng) =
        Gen
          .union(g1, g2)
          .sample
          .run(RNG.Simple(1))
      expected should equal(2)
    }
  }

  "weighted" - {
    "指定した重みで値を生成できること" in {
      val g1 = (Gen.unit(1), 0.75)
      val g2 = (Gen.unit(2), 0.25)
      val (expected, rng) =
        Gen
          .weighted(g1, g2)
          .listOfN(Gen.unit(100))
          .sample
          .run(RNG.Simple(1))
      expected.count(n => n == 1) should equal(75)
      expected.count(n => n == 2) should equal(25)
    }
  }

  "&&" - {
    import fpinscala.testing.Prop.*
    "false = false && false" in {
      val p1 = Prop((testCases: TestCases, rng: RNG) => Falsified("test", 1))
      val p2 = Prop((testCases: TestCases, rng: RNG) => Falsified("test", 1))
      p1 && p2 should equal(false)
    }
    "false = true && false" in {
      val p1 = Prop((testCases: TestCases, rng: RNG) => Passed)
      val p2 = Prop((testCases: TestCases, rng: RNG) => Falsified("test", 1))
      p1 && p2 should equal(false)
    }
    "false = false && true" in {
      val p1 = Prop((testCases: TestCases, rng: RNG) => Falsified("test", 1))
      val p2 = Prop((testCases: TestCases, rng: RNG) => Passed)
      p1 && p2 should equal(false)
    }
    "true = true && true" in {
      val p1 = Prop((testCases: TestCases, rng: RNG) => Passed)
      val p2 = Prop((testCases: TestCases, rng: RNG) => Passed)
      p1 && p2 should equal(true)
    }
  }
  "||" - {
    import fpinscala.testing.Prop.*
    "false = false && false" in {
      val p1 = Prop((testCases: TestCases, rng: RNG) => Falsified("test", 1))
      val p2 = Prop((testCases: TestCases, rng: RNG) => Falsified("test", 1))
      p1 || p2 should equal(false)
    }
    "true = true && false" in {
      val p1 = Prop((testCases: TestCases, rng: RNG) => Passed)
      val p2 = Prop((testCases: TestCases, rng: RNG) => Falsified("test", 1))
      p1 || p2 should equal(false)
    }
    "true = false && true" in {
      val p1 = Prop((testCases: TestCases, rng: RNG) => Falsified("test", 1))
      val p2 = Prop((testCases: TestCases, rng: RNG) => Passed)
      p1 || p2 should equal(false)
    }
    "true = true && true" in {
      val p1 = Prop((testCases: TestCases, rng: RNG) => Passed)
      val p2 = Prop((testCases: TestCases, rng: RNG) => Passed)
      p1 || p2 should equal(true)
    }
  }
