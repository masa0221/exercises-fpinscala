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

  "filter" - {
    "Noneの場合はNoneになること" in {
      None.filter(a => true) `shouldBe` None
    }
    "Someの場合" - {
      "条件が一致した時はSomeになること" in {
        Some(4).filter(_ == 4) `shouldBe` Some(4)
      }
      "条件が一致しない時はSomeになること" in {
        Some(4).filter(_ == 5) `shouldBe` None
      }
    }
  }

  "variance" - {
    "値がない場合はNoneを返すこと" in {
      Option.variance(Seq()) `shouldBe` None
    }
    "分散の計算が正常に行えること" in {
      Option.variance(Seq(4, 1, 3, 6, 4, 3)) `shouldBe` Some(2.25)
    }
  }

  "map2" - {
    "aがNoneの場合はNoneを返すこと" in {
      Option.map2(None, Some(1))((a, b) => "failed") `shouldBe` None
    }
    "bがNoneの場合はNoneを返すこと" in {
      Option.map2(Some(1), None)((a, b) => "failed") `shouldBe` None
    }
    "aとb両方Someの場合は関数の結果が返されること返すこと" in {
      Option.map2(Some(1), Some(2))((a, b) => a + b) `shouldBe` Some(3)
    }
  }

  "sequence" - {
    "リストの値が全てSomeの場合は結合されること" in {
      Option.sequence(List(Some(1), Some(2), Some(3))) `shouldBe` Some(
        List(1, 2, 3)
      )
    }
    "リストの中に一つでもNoneがある場合は結果がNoneになること" in {
      Option.sequence(List(Some(1), None, Some(3))) `shouldBe` None
    }
  }

  "traverse" - {
    "リストの値が全てSomeの場合は結合されること" in {
      Option.traverse(List(1, 2, 3))(a => Some(a.toString)) `shouldBe` Some(
        List("1", "2", "3")
      )
    }
    "リストの中に一つでもNoneがある場合は結果がNoneになること" in {
      Option.traverse(List(0, 1, 2, 3))(a =>
        if (a == 0) None else Some(a.toString)
      ) `shouldBe` None
    }
  }

end OptionTest
