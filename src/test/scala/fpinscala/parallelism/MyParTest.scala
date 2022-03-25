package fpinscala.parallelism

import fpinscala.parallelism.MyPar.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class MyParTest extends AnyFreeSpecLike with Matchers:
  "MyExamples" - {
    "sum" - {
      "リストの合計を取得できること" in {
        val myPar = MyExamples.sum(IndexedSeq(1, 2, 3, 4, 5))
        MyPar.run(myPar) should equal(15)
      }
    }
  }
end MyParTest
