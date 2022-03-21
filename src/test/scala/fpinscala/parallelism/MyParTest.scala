package fpinscala.parallelism

import fpinscala.parallelism.MyPar.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class MyParTest extends AnyFreeSpecLike with Matchers:
  "sum" - {
    "リストの合計を取得できること" in {
      MyPar.sum(IndexedSeq(1, 2, 3, 4, 5)) should equal(15)
    }
  }
end MyParTest
