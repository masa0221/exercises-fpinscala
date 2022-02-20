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

  "takeWhileViaFoldRight" - {
    "指定した関数がtrueの値だけのリストになっていること" in {
      LazyList(1, 2, 3, 4, 5)
        .takeWhileViaFoldRight(_ % 2 == 0)
        .toList `shouldBe` List(2, 4)
    }
  }

  "headOption" - {
    "空のリストの場合はNoneになること" in {
      LazyList().headOption `shouldBe` None
    }
    "リストがある場合はリスト先頭の値がSomeで取得できること" in {
      LazyList(1, 2, 3, 4, 5).headOption `shouldBe` Some(1)
    }
  }

  "map" - {
    "リストに対して関数が適用されていること" in {
      LazyList(1, 2, 3, 4, 5).map(_ * 2).toList `shouldBe` List(2, 4, 6, 8, 10)
    }
  }

  "filter" - {
    "指定した関数がtrueの値だけのリストになっていること" in {
      LazyList(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList `shouldBe` List(2, 4)
    }
  }

  "append" - {
    "指定したリストを末尾に追加できること" in {
      LazyList(1, 2, 3, 4, 5)
        .append(LazyList(6, 7, 8, 9, 10))
        .toList `shouldBe` List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    }
  }

  "flatMap" - {
    "指定した関数が適用されて文脈が同じリストになること" in {
      LazyList(1, 2, 3, 4, 5)
        .flatMap(a => LazyList(a, a + 1))
        .toList `shouldBe` List(1, 2, 2, 3, 3, 4, 4, 5, 5, 6)
    }
  }

  "continually" - {
    "無限ストリームを操作できること" in {
      LazyList.continually(1).take(3).toList `shouldBe` List(1, 1, 1)
    }
  }

  "from" - {
    "指定した値から1ずつ値が加算される無限ストリームを操作できること" in {
      LazyList.from(2).take(5).toList `shouldBe` List(2, 3, 4, 5, 6)
    }
  }

  "unfold" - {
    "unfoldを使ってフィボナッチ数列ができること" in {
      val actual = LazyList.unfold((0, 1)) {
        case (a, b) if b <= 13 => Some((b, (b, a + b)))
        case _                 => None
      }
      actual.toList `shouldBe` List(1, 1, 2, 3, 5, 8, 13)
    }
  }

  "zipWith" - {
    "指定した関数が適応された値のリストになること" in {
      LazyList(1, 2, 3, 4, 5)
        .zipWith(LazyList(1, 2, 3, 4, 5), (a, b) => a + b)
        .toList `shouldBe` List(2, 4, 6, 8, 10)
    }
    "指定したリストの長さに合わせたリストになること" - {
      "元のリストが少ない場合" in {
        LazyList(1, 2, 3)
          .zipWith(LazyList(1, 2, 3, 4, 5), (a, b) => a + b)
          .toList `shouldBe` List(2, 4, 6)
      }
      "指定したリストが少ない場合" in {
        LazyList(1, 2, 3, 4, 5)
          .zipWith(LazyList(1, 2, 3), (a, b) => a + b)
          .toList `shouldBe` List(2, 4, 6)
      }
    }
  }

  "zipAll" - {
    "元のリストより指定したリストが多い場合は、指定したリストの数に合わせたタプルのリストになること" in {
      LazyList(1, 2, 3).zipAll(LazyList(1, 2, 3, 4, 5)).toList `shouldBe` List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (Some(3), Some(3)),
        (None, Some(4)),
        (None, Some(5))
      )
    }
    "元のリストより指定したリストが少ない場合は、元のリストの数に合わせたタプルのリストになること" in {
      LazyList(1, 2, 3, 4, 5).zipAll(LazyList(1, 2, 3)).toList `shouldBe` List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (Some(3), Some(3)),
        (Some(4), None),
        (Some(5), None)
      )
    }
    "元のリストと指定したリストが同じ数の場合は、値が全てSomeのタプルのリストになること" in {
      LazyList(1, 2, 3, 4, 5)
        .zipAll(LazyList(1, 2, 3, 4, 5))
        .toList `shouldBe` List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (Some(3), Some(3)),
        (Some(4), Some(4)),
        (Some(5), Some(5))
      )
    }
  }
end LazyListTest
