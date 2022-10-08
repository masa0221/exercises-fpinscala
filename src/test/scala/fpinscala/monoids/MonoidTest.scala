package fpinscala.monoids

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import java.util.concurrent.Executors

class MonoidTest extends AnyFreeSpecLike with Matchers:
  import Monoid.*

  "stringMonoid" - {
    "足し算ができること" in {
      stringMonoid.op("abcd", "efg") should equal("abcdefg")
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
  "optionMonoid" - {
    val x = Some(1)
    val y = Some(2)
    val z = Some(3)

    val o = Monoid.optionMonoid[Int]
    val zero = o.zero

    "op(op(x, y), z) == op(x, op(y, z))" in {
      o.op(o.op(x, y), z) should equal(o.op(x, o.op(y, z)))
    }
    "op(x, zero) == x" in {
      o.op(x, zero) should equal(x)
    }
    "op(zero, x) == x" in {
      o.op(zero, x) should equal(x)
    }
  }

  "endoMonoid" - {
    val x = (a: Int) => a + 1
    val y = (a: Int) => a + 2
    val z = (a: Int) => a + 3

    val o = Monoid.endoMonoid[Int]
    val zero = o.zero

    "op(op(x, y), z) == op(x, op(y, z))" in {
      o.op(o.op(x, y), z) === equal(o.op(x, o.op(y, z)))
    }
    "op(x, zero) == x" in {
      o.op(x, zero) === equal(x(1))
    }
    "op(zero, x) == x" in {
      o.op(zero, x) === equal(x)
    }

  }

  "monoidLaws" in {
    import fpinscala.answers.testing.exhaustive.Gen
    import fpinscala.answers.testing.exhaustive.Prop
    import fpinscala.answers.testing.exhaustive.Prop.Result.Passed

    val genInt = Gen.choose(Int.MinValue, Int.MaxValue)
    val genOption = genInt.map(i => if i % 2 == 0 then Some(i / 2) else None)

    monoidLaws(intAddition, genInt).check() === equal(Passed)
    monoidLaws(intMultiplication, genInt).check() === equal(Passed)
    monoidLaws(booleanOr, Gen.boolean).check() === equal(Passed)
    monoidLaws(booleanAnd, Gen.boolean).check() === equal(Passed)
    monoidLaws(optionMonoid[Int], genOption).check() === equal(Passed)
  }

  "concatenate" in {
    concatenate(List("a", "b", "c"), stringMonoid) should equal("abc")
  }

  "flatMap" in {
    flatMap(List(1.1, 2.2, 3.3), intAddition)(_.toInt) should equal(6)
  }

  "foldLeft" in {
    foldLeft(List(1, 2, 3))(intAddition.zero)(
      intAddition.op
    ) should equal(6)
  }

  "foldRight" in {
    foldLeft(List(1, 2, 3))(intAddition.zero)(
      intAddition.op
    ) should equal(6)
  }

  "foldMapV" in {
    val m = intAddition
    foldMapV(IndexedSeq(1, 2, 3, 4), m)(_ + 1) should equal(14)
  }

  "parFoldMap" in {
    val m = intAddition
    val p = parFoldMap(IndexedSeq(1, 2, 3, 4), m)(_ + 1)
    val es = Executors.newFixedThreadPool(2)
    p.run(es) should equal(14)
  }

  "ordered" in {
    ordered(IndexedSeq(1, 2, 3, 4, 5)) should equal(true)
    ordered(IndexedSeq(3, 5, 1, 2, 4)) should equal(false)
  }

  "wcMonoid" - {
    "Stubのみ" - {
      val m = Monoid.wcMonoid
      val x = Stub("x")
      val y = Stub("y")
      val z = Stub("z")
      monoidTest[WC](m, x, y, z)
    }

    "Partのみ" - {
      val m = Monoid.wcMonoid
      val x = Part("x1", 1, "x2")
      val y = Part("y1", 2, "y2")
      val z = Part("z1", 3, "y3")
      monoidTest[WC](m, x, y, z)
    }

    "StubとPartどちらもある場合" - {
      val m = Monoid.wcMonoid
      val x = Stub("x")
      val y = Part("y1", 2, "y2")
      val z = Stub("z")
      monoidTest[WC](m, x, y, z)
    }

    def monoidTest[A](m: Monoid[A], x: A, y: A, z: A): Unit =
      val zero = m.zero

      "op(op(x, y), z) == op(x, op(y, z))" in {
        m.op(m.op(x, y), z) === equal(m.op(x, m.op(y, z)))
      }
      "op(x, zero) == x" in {
        m.op(x, zero) === equal(x)
      }
      "op(zero, x) == x" in {
        m.op(zero, x) === equal(x)
      }
  }

  "count" in {
    count("What is WC meaning?") should equal(4)
    count("It's toilet meaning in Japan.") should equal(5)
    count("Please tell me why I writing tests for toilet.") should equal(9)
  }

  "Foldable" - {
    "FoldableList" - {
      "foldRight" in {
        val as = List(1, 2, 3, 4, 5)
        val f = (a: Int, b: String) => a.toString + b
        FoldableList.foldRight(as)("")(f) should equal("12345")
      }
      "foldLeft" in {
        val as = List(1, 2, 3, 4, 5)
        val f = (b: String, a: Int) => a.toString + b
        FoldableList.foldLeft(as)("")(f) should equal("54321")
      }
      "foldMap" in {
        val as = List(1, 2, 3, 4, 5)
        FoldableList.foldMap(as)(_ * 2)(intAddition) should equal(30)
      }
      "concatenate" in {
        val as = List(1, 2, 3, 4, 5)
        FoldableList.concatenate(as)(intAddition) should equal(15)
      }
    }
    "FoldableIndexedSeq" - {
      "foldRight" in {
        val as = IndexedSeq(1, 2, 3, 4, 5)
        val f = (a: Int, b: String) => a.toString + b
        FoldableIndexedSeq.foldRight(as)("")(f) should equal("12345")
      }
      "foldLeft" in {
        val as = IndexedSeq(1, 2, 3, 4, 5)
        val f = (b: String, a: Int) => a.toString + b
        FoldableIndexedSeq.foldLeft(as)("")(f) should equal("54321")
      }
      "foldMap" in {
        val as = IndexedSeq(1, 2, 3, 4, 5)
        FoldableIndexedSeq.foldMap(as)(_ * 2)(intAddition) should equal(30)
      }
      "concatenate" in {
        val as = IndexedSeq(1, 2, 3, 4, 5)
        FoldableIndexedSeq.concatenate(as)(intAddition) should equal(15)
      }
    }
  }
