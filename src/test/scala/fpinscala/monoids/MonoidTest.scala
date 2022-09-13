package fpinscala.monoids

import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

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
