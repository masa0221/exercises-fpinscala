package fpinscala.testing

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import fpinscala.state.RNG
import org.scalactic.Pass
import fpinscala.testing.Prop.Passed
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import fpinscala.parallelism.MyPar
import fpinscala.parallelism.Par
import fpinscala.testing.Prop.{Falsified, Passed}

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
    val rng = RNG.Simple(1)
    "false = false && false" in {
      val p1 = Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
        Falsified("test1", 1)
      )
      val p2 = Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
        Falsified("test2", 1)
      )
      val actual = (p1 && p2).run(1, 1, rng)
      actual should equal(Falsified("test1", 1))
    }
    "false = true && false" in {
      val p1 =
        Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) => Passed)
      val p2 = Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
        Falsified("test2", 1)
      )
      val actual = (p1 && p2).run(1, 1, rng)
      actual should equal(Falsified("test2", 1))
    }
    "false = false && true" in {
      val p1 = Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
        Falsified("test1", 1)
      )
      val p2 =
        Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) => Passed)
      val actual = (p1 && p2).run(1, 1, rng)
      actual should equal(Falsified("test1", 1))
    }
    "true = true && true" in {
      val p1 =
        Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) => Passed)
      val p2 =
        Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) => Passed)
      val actual = (p1 && p2).run(1, 1, rng)
      actual should equal(Passed)
    }
  }
  "||" - {
    import fpinscala.testing.Prop.*
    val rng = RNG.Simple(1)
    "false = false && false" in {
      val p1 = Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
        Falsified("test1", 1)
      )
      val p2 = Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
        Falsified("test2", 1)
      )
      val actual = (p1 || p2).run(1, 1, rng)
      actual should equal(Falsified("test2", 1))
    }
    "true = true && false" in {
      val p1 =
        Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) => Passed)
      val p2 = Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
        Falsified("test2", 1)
      )
      val actual = (p1 || p2).run(1, 1, rng)
      actual should equal(Passed)
    }
    "true = false && true" in {
      val p1 = Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
        Falsified("test1", 1)
      )
      val p2 =
        Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) => Passed)
      val actual = (p1 || p2).run(1, 1, rng)
      actual should equal(Passed)
    }
    "true = true && true" in {
      val p1 =
        Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) => Passed)
      val p2 =
        Prop((maxSize: MaxSize, testCases: TestCases, rng: RNG) => Passed)
      val actual = (p1 || p2).run(1, 1, rng)
      actual should equal(Passed)
    }
  }
  "unsized" - {
    "GenからSGenが生成できること" in {}
  }

  "nonEmptyList" - {
    "Passすること" in {
      val smallInt = Gen.choose(-10, 10)
      val maxProp = Prop.forAll(smallInt.nonEmptyList) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }

      maxProp.run(1, 1, RNG.Simple(1)) should equal(Passed)
    }
  }

  "sortedProp" - {
    "Passすること" in {
      val smallInt = Gen.choose(-10, 10)
      // TODO: ちゃんと理解する
      // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/testing/14.answer.md
      val sortedProp = Prop.forAll(smallInt.list) { list =>
        val slist = list.sorted
        val ordered =
          list.isEmpty || slist.zip(slist.tail).forall { (a, b) => a <= b }
        ordered && list.forall(slist.contains) && slist.forall(list.contains)
      }

      sortedProp.run(1, 1, RNG.Simple(1)) should equal(Passed)
    }
  }

  "Parのテスト" - {
    val es = Executors.newFixedThreadPool(2)
    "forAll" in {
      val actual = Prop.forAll(Gen.unit(MyPar.unit(1)))(i =>
        MyPar.run(es)(i).get == MyPar.run(es)(MyPar.unit(2)).get
      )
      actual.run(1, 1, RNG.Simple(1)) shouldBe a[Falsified]
    }
    "check1" in {
      val actual = Prop.check {
        val p1 = MyPar.map(MyPar.unit(1))(_ + 1)
        val p2 = MyPar.unit(2)
        MyPar.run(es)(p1).get == MyPar.run(es)(p2).get
      }
      actual.run(1, 1, RNG.Simple(1)) should equal(Passed)
    }
    "check2" in {
      val actual = Prop.check {
        val p1 = MyPar.map(MyPar.unit(1))(_ + 1)
        val p2 = MyPar.unit(2)
        MyPar.run(es)(Prop.equalPars(p1, p2)).get
      }
      actual.run(1, 1, RNG.Simple(1)) should equal(Passed)
    }
    "forAllPar" in {
      val actual =
        Prop.forAllPar(Gen.unit(MyPar.unit(1)))(a =>
          MyPar.map(MyPar.unit(1))(a => a == 1)
        )
      actual.run(1, 1, RNG.Simple(1)) should equal(Passed)
    }
  }
