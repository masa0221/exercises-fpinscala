package fpinscala.parallelism

import fpinscala.parallelism.MyNonblocking.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import java.util.concurrent.Executors

class MyNonblockingTest extends AnyFreeSpecLike with Matchers:
  "MyPar" - {
    val es = Executors.newFixedThreadPool(2)

    "map2" - {
      "2つのMyParを受け取って関数を適用できること" in {
        val p1 = MyPar.unit(1)
        val p2 = MyPar.unit(2)
        val p3 = MyPar.map2(p1, p2)(_ + _)
        MyPar.run(es)(p3) should equal(3)
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
        MyPar.run(es)(myPar) should equal(
          List(2, 4, 6, 8, 10)
        )
      }
    }
  }
end MyNonblockingTest
