package fpinscala.parallelism

import fpinscala.parallelism.MyPar.*
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
        var parList = List(
          MyPar.unit(1),
          MyPar.unit(2),
          MyPar.unit(3),
          MyPar.unit(4),
          MyPar.unit(5)
        )
        var sequencedPar = MyPar.sequence(parList)
        MyPar.run(es)(sequencedPar) should equal(
          MyPar.run(es)(MyPar.unit(List(1, 2, 3, 4, 5)))
        )
      }
    }
    "parMap" - {
      "指定した関数が適用されるMyParが取得できること" in {
        var list = List(1, 2, 3, 4, 5)
        var myPar = MyPar.parMap(list)(_ * 2)
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(
          List(2, 4, 6, 8, 10)
        )
      }
    }
  }
end MyParTest
