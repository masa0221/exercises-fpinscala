package fpinscala.laziness

import fpinscala.laziness.LazyList.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class LazyListTest extends AnyFreeSpecLike with Matchers:
  "toList" - {
    "値が展開されたListを取得できること" in {
      LazyList(1, 2, 3, 4, 5).toList `shouldBe` List(1, 2, 3, 4, 5)
    }
  }

  "take" - {
    "指定した数だけリストの先頭から取得できること" in {
      LazyList(1, 2, 3, 4, 5).take(3).toList `shouldBe` List(1, 2, 3)
    }
  }

  "drop" - {
    "指定した数だけリストの先頭から取得できること" in {
      LazyList(1, 2, 3, 4, 5).drop(3).toList `shouldBe` List(4, 5)
    }
  }

end LazyListTest
