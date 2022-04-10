package fpinscala.parallelism

import fpinscala.parallelism.MyNonblocking.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import java.util.concurrent.Executors

class MyNonblockingTest extends AnyFreeSpecLike with Matchers:
  "MyPar" - {
    val es = Executors.newFixedThreadPool(2)

    "unit" in {
      MyPar.run(es)(MyPar.unit(1)) should equal(1)
    }
    "map2" in {
      val p1 = MyPar.unit(1)
      val p2 = MyPar.unit(2)
      val p3 = MyPar.map2(p1, p2)(_ + _)
      MyPar.run(es)(p3) should equal(3)
    }
  }
end MyNonblockingTest
