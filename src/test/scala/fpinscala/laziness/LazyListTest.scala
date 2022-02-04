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

  "takeWhile" - {
    "指定した関数がtrueの値だけのリストになっていること" in {
      LazyList(1, 2, 3, 4, 5).takeWhile(_ % 2 == 0).toList `shouldBe` List(2, 4)
    }
  }

  "forAll" - {
    "指定した関数が一つでもfalseになる値を持っている場合はfalse" in {
      LazyList(1, 2, 3, 4, 5).forAll(_ == 3) `shouldBe` false
    }
    "指定した関数が全てtrueになる場合はtrue" in {
      LazyList(1, 2, 3, 4, 5).forAll(_ < 6) `shouldBe` true
    }
  }
end LazyListTest
