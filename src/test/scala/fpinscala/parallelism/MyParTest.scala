package fpinscala.parallelism

import fpinscala.parallelism.MyPar.*
import fpinscala.parallelism.MyPar
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import org.scalactic.Prettifier.default

class MyParTest extends AnyFreeSpecLike with Matchers:
  "MyExamples" - {
    val es = Executors.newFixedThreadPool(2)
    "sum" - {
      "リストの合計を取得できること" in {
        val myPar = MyExamples.sum(IndexedSeq(1, 2, 3, 4, 5))
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(15)
      }
    }
    "asyncF" - {
      "関数が適用できること" in {
        val myPar = MyPar.asyncF[Int, Int](_ + 1)(1)
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(2)
      }
    }
    "sortPar" - {
      "リストをソートできること" in {
        val myPar = MyPar.sortPar(MyPar.unit(List(4, 2, 3, 1, 5)))
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(
          List(1, 2, 3, 4, 5)
        )
      }
    }
    "map" - {
      "指定した関数が適用できること" in {
        val myPar = MyPar.map(MyPar.unit(2))(_ + 3)
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(5)
      }
    }
    "sequence" - {
      "MyParのリストがMyPar型に変換されること" in {
        val parList = List(
          MyPar.unit(1),
          MyPar.unit(2),
          MyPar.unit(3),
          MyPar.unit(4),
          MyPar.unit(5)
        )
        val sequencedPar = MyPar.sequence(parList)
        MyPar.run(es)(sequencedPar) should equal(
          MyPar.run(es)(MyPar.unit(List(1, 2, 3, 4, 5)))
        )
      }
    }
    "parMap" - {
      "指定した関数が適用されるMyParが取得できること" in {
        val list = List(1, 2, 3, 4, 5)
        val myPar = MyPar.parMap(list)(_ * 2)
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(
          List(2, 4, 6, 8, 10)
        )
      }
    }
    "parFilter" - {
      "指定したリストに対して、同じく指定した関数でフィルタリングされたMyParが取得できる" in {
        val list = List(1, 2, 3, 4, 5)
        val myPar = MyPar.parFilter(list)(_ % 2 == 0)
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(
          List(2, 4)
        )
      }
    }
    "equal" - {
      "二つの同じ値を持ったMyParの場合trueになること" in {
        val p1 = MyPar.unit(1)
        val p2 = MyPar.unit(1)
        MyPar.equal(es)(p1, p2) should equal(true)

      }
      "二つの異なる値を持ったMyParの場合falseになること" in {
        val p1 = MyPar.unit(1)
        val p2 = MyPar.unit(2)
        MyPar.equal(es)(p1, p2) should equal(false)
      }
    }
    "delay" - {
      // "Example. Occur dead lock" in {
      //   val a = lazyUnit(42 + 1)
      //   val S = Executors.newFixedThreadPool(1)
      //   MyPar.equal(S)(a, fork(a)) should equal(true)
      // }
      "デッドロックが発生しないこと" in {
        val a = lazyUnit(42 + 1)
        val S = Executors.newFixedThreadPool(1)
        MyPar.equal(S)(a, delay(a)) should equal(true)
      }
    }

    "choice" - {
      "指定した条件がtrueの時は1つ目のMyParを実行すること" in {
        val cond = MyPar.unit(true)
        val p1 = MyPar.unit(1)
        val p2 = MyPar.unit(2)
        MyPar
          .run(es)(MyPar.choice(cond)(p1, p2))
          .get(1L, TimeUnit.SECONDS) should equal(1)
      }
      "指定した条件がfalseの時は2つ目のMyParを実行すること" in {
        val cond = MyPar.unit(false)
        val p1 = MyPar.unit(1)
        val p2 = MyPar.unit(2)
        MyPar
          .run(es)(MyPar.choice(cond)(p1, p2))
          .get(1L, TimeUnit.SECONDS) should equal(2)
      }
    }
    "choiceN" - {
      "指定した数字のMyParを実行すること" in {
        val n = MyPar.unit(1)
        val pl = List(
          MyPar.unit(1),
          MyPar.unit(2),
          MyPar.unit(3)
        )
        MyPar
          .run(es)(MyPar.choiceN(n)(pl))
          .get(1L, TimeUnit.SECONDS) should equal(2)
      }
    }

    "choiceMap" - {
      "指定したキーの値をMapから取得できること" in {
        val key = MyPar.unit("b")
        val pm = Map(
          "a" -> MyPar.unit(1),
          "b" -> MyPar.unit(2),
          "c" -> MyPar.unit(3)
        )
        MyPar
          .run(es)(MyPar.choiceMap(key)(pm))
          .get(1L, TimeUnit.SECONDS) should equal(2)
      }
    }

    "flatMap" - {
      "指定した関数を適用できること" in {
        val a = MyPar.unit("b")
        var b = (a: String) =>
          a match
            case "a" => MyPar.unit(1)
            case "b" => MyPar.unit(2)
            case "c" => MyPar.unit(3)
            case _   => MyPar.unit(4)

        MyPar
          .run(es)(MyPar.flatMap(a)(b))
          .get(1L, TimeUnit.SECONDS) should equal(2)
      }
    }
  }
end MyParTest
