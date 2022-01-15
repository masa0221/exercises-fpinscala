package fpinscala.errorhandling

import fpinscala.errorhandling.Option.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class OptionTest extends AnyFreeSpecLike with Matchers:
  "map" - {
    "Noneの場合はNoneになること" in {
      None.map(a => a) `shouldBe` None
    }
    "Someの場合は関数が適応されること" in {
      Some(10).map(_ * 10) `shouldBe` Some(100)
    }
  }

  "getOrElse" - {
    "Noneの場合は引数の値が取得できること" in {
      None.getOrElse(100) `shouldBe` 100
    }
    "Someの場合はSomeが持っている値が取得できること" in {
      Some(1).getOrElse(100) `shouldBe` 1
    }
  }

  "flatMap" - {
    "Noneの場合は引数の値が取得できること" in {
      None.flatMap(a => a) `shouldBe` None
    }
    "Someの場合はSomeが持っている値が取得できること" in {
      Some(2).flatMap(a => Some(a * 100)) `shouldBe` Some(200)
    }
  }

  "orElse" - {
    "Noneの場合は引数の値が取得できること" in {
      None.orElse(Some(3)) `shouldBe` Some(3)
    }
    "Someの場合はSomeが持っている値が取得できること" in {
      Some(4).orElse(Some(5)) `shouldBe` Some(4)
    }
  }

end OptionTest
