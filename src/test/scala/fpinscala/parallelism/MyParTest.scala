package fpinscala.parallelism

import fpinscala.parallelism.MyPar.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

class MyParTest extends AnyFreeSpecLike with Matchers:
  "MyExamples" - {
    "sum" - {
      "リストの合計を取得できること" in {
        val es = Executors.newFixedThreadPool(2)
        val myPar = MyExamples.sum(IndexedSeq(1, 2, 3, 4, 5))
        MyPar.run(es)(myPar).get(1L, TimeUnit.SECONDS) should equal(15)
      }
    }
  }
end MyParTest
