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
        val es = Executors.newFixedThreadPool(2)
        val myPar = MyPar.asyncF[Int, Int](_ + 1)(1)
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(2)
      }
    }
  }
end MyParTest
