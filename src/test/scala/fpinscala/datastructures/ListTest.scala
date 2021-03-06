package fpinscala.datastructures

import fpinscala.datastructures.List
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFreeSpecLike with Matchers:
  val listInt = List(1, 2, 3)
  val listDouble = List(1.0, 2.0, 3.0)
  val listString = List("one", "two", "three")

  "tail" - {
    "先頭の要素を削除できること" in {
      List.tail(listInt) `shouldBe` List(2, 3)
      List.tail(listDouble) `shouldBe` List(2.0, 3.0)
      List.tail(listString) `shouldBe` List("two", "three")
    }
  }

  "setHead" - {
    "Listが空でもでも正常に動くこと" in {
      List.setHead(List(), 0) `shouldBe` List(0)
    }

    "先頭に要素を追加できること" in {
      List.setHead(listInt, 0) `shouldBe` List(0, 1, 2, 3)
      List.setHead(listDouble, 0) `shouldBe` List(0, 1.0, 2.0, 3.0)
      List.setHead(listString, "zero") `shouldBe` List(
        "zero",
        "one",
        "two",
        "three"
      )
    }
  }

  "drop" - {
    "Listが空でもでも正常に動くこと" in {
      List.drop(List(), 1) `shouldBe` List()
    }

    "存在しない番号を指定した時は変化がないこと" in {
      List.drop(listInt, 0) `shouldBe` List(1, 2, 3)
      List.drop(listInt, -1) `shouldBe` List(1, 2, 3)
    }

    "指定した番号の要素を削除できること" in {
      List.drop(listInt, 2) `shouldBe` List(1, 3)
      List.drop(listDouble, 2) `shouldBe` List(1.0, 3.0)
      List.drop(listString, 2) `shouldBe` List("one", "three")
    }
  }

  "dropWhile" - {
    "条件に一致する値だけ削除できること" in {
      List.dropWhile(listInt, v => v == 1) `shouldBe` List(2, 3)
      List.dropWhile(listDouble, v => v == 2.0) `shouldBe` List(1.0, 3.0)
      List.dropWhile(listString, v => v == "three") `shouldBe` List(
        "one",
        "two"
      )
    }

    "条件に一致しない場合は変化がないこと" in {
      List.dropWhile(listInt, v => v == 4) `shouldBe` List(1, 2, 3)
    }
  }

  "init" - {
    "末尾を除いたリストが取得できること" in {
      List.init(listInt) `shouldBe` List(1, 2)
      List.init(listDouble) `shouldBe` List(1.0, 2.0)
      List.init(listString) `shouldBe` List("one", "two")
    }

    "リストが空でも正常に動くこと" in {
      List.init(List()) `shouldBe` List()
    }
  }

  "length" - {
    "リストが空の時は0を返すこと" in {
      List.length(List()) `shouldBe` 0
    }

    "リストに一つ以上値がある場合は正しい値を返すこと" in {
      List.length(List(1)) `shouldBe` 1
      List.length(List(1, 2, 3)) `shouldBe` 3
    }
  }

  "foldLeft" - {
    "foldLeftが意図通りに動くこと" in {
      List.foldLeft(List(1, 2, 3, 4, 5), 0, _ + _) `shouldBe` 15
      List.foldLeft(List(1, 2, 3, 4, 5), 1, _ * _) `shouldBe` 120
    }
  }

  "sumViaFoldLeft" - {
    "リストの合計が出せること" in {
      List.foldLeft(List(1, 2, 3, 4, 5), 0, _ + _) `shouldBe` 15
    }
  }

  "productViaFoldLeft" - {
    "リストの積が出せること" in {
      List.foldLeft(List(1, 2, 3, 4, 5), 1, _ * _) `shouldBe` 120
    }
  }

  "lengthViaFoldLeft" - {
    "リストが空の時は0を返すこと" in {
      List.length(List()) `shouldBe` 0
    }

    "リストに一つ以上値がある場合は正しい値を返すこと" in {
      List.length(List(1)) `shouldBe` 1
      List.length(List(1, 2, 3)) `shouldBe` 3
    }
  }

  "reverse" - {
    "逆順のリストが取得できること" in {
      List.reverse(List(1, 2, 3, 4, 5)) `shouldBe` List(5, 4, 3, 2, 1)
    }

    "リストが空でも正常に動くこと" in {
      List.reverse(List()) `shouldBe` List()
    }
  }

  "foldRightViaFoldLeft" - {
    "リストの合計が出せること" in {
      List.foldRightViaFoldLeft(List(1, 2, 3, 4, 5), 0, _ + _) `shouldBe` 15
    }
  }

  "foldLeftViaFoldRight" - {
    "リストの合計が出せること" in {
      List.foldLeftViaFoldRight(List(1, 2, 3, 4, 5), 0, _ + _) `shouldBe` 15
    }
  }

  "appendViaFoldRight" - {
    "リストとリストを結合できること" in {
      List.appendViaFoldRight(List(1, 2, 3), List(4, 5, 6))
      `shouldBe` List (1, 2, 3, 4, 5, 6)
    }
  }

  "concat" - {
    "リストの中にある複数のリストが結合できること" in {
      List.concat(List(List(1, 2, 3), List(4, 5, 6)))
      `shouldBe` List (1, 2, 3, 4, 5, 6)
    }
  }

  "incrementEach" - {
    "リストの値が1ずつ増えること" in {
      List.incrementEach(List(1, 2, 3, 4, 5)) `shouldBe` List(2, 3, 4, 5, 6)
    }
  }

  "doubleToString" - {
    "リストの値が文字列になること" in {
      List.doubleToString(List(1.0, 2.0, 3.0, 4.0, 5.0)) `shouldBe`
        List("1.0", "2.0", "3.0", "4.0", "5.0")
    }
  }

  "map" - {
    "リストの値を一つずつ処理できること" in {
      List.map(List(1, 2, 3, 4, 5))(a => a * a) `shouldBe`
        List(1, 4, 9, 16, 25)
    }
  }

  "filter" - {
    "リストをフィルターできること" in {
      List.filter(List(1, 2, 3, 4, 5), _ % 2 == 0) `shouldBe`
        List(2, 4)
    }
  }

  "filterViaFlatMap" - {
    "リストをフィルターできること" in {
      List.filterViaFlatMap(List(1, 2, 3, 4, 5), _ % 2 == 0) `shouldBe`
        List(2, 4)
    }
  }

  "flatMap" - {
    "リストをflatMapで処理できること" in {
      List.flatMap(List(1, 2, 3, 4, 5), a => List(a.toString)) `shouldBe`
        List("1", "2", "3", "4", "5")
      List.flatMap(List(1, 2, 3), i => List(i, i)) `shouldBe`
        List(1, 1, 2, 2, 3, 3)
    }
  }

  "addPairwise" - {
    "リスト同士の値の和のリストになること" in {
      List.addPairwise(List(1, 2, 3), List(4, 5, 6)) `shouldBe`
        List(5, 7, 9)
    }
  }

  "zipWith" - {
    "リスト同士の値の和のリストになること" in {
      List.zipWith(List(1, 2, 3), List(4, 5, 6), _ + _) `shouldBe`
        List(5, 7, 9)
    }

    "リスト同士の値の積のリストになること" in {
      List.zipWith(List(1, 2, 3), List(4, 5, 6), _ * _) `shouldBe`
        List(4, 10, 18)
    }

    "リストの値が文字でも動くこと" in {
      List.zipWith(List("l", "s"), List("i", "t"), _ + _) `shouldBe`
        List("li", "st")
    }

    "リストの値が異なる型でも動くこと" in {
      List.zipWith(List(1, 2, 3), List("4", "5", "6"), _ + _.toInt) `shouldBe`
        List(5, 7, 9)
    }
  }

  "startsWith" - {
    "指定した値が空ならtrueになること" in {
      val list = List(1, 2, 3, 4, 5)
      List.startsWith(list, List()) `shouldBe` true
    }

    "リストの先頭が指定した値ならtrueになること" in {
      val list = List(1, 2, 3, 4, 5)
      List.startsWith(list, List(1, 2, 3, 4, 5)) `shouldBe` true
      List.startsWith(list, List(1)) `shouldBe` true
    }

    "リストの先頭が指定した値でなければfalseになること" in {
      val list = List(1, 2, 3, 4, 5)
      List.startsWith(list, List(2, 3)) `shouldBe` false
      List.startsWith(list, List(2, 3, 4)) `shouldBe` false
      List.startsWith(list, List(3, 4)) `shouldBe` false
      List.startsWith(list, List(4, 5)) `shouldBe` false
      List.startsWith(list, List(5)) `shouldBe` false
    }
  }

  "hasSubsequence" - {
    "リストの中にサブシーケンスがある場合はtrueになること" in {
      val list = List(1, 2, 3, 4, 5)
      List.hasSubsequence(list, List(1, 2, 3, 4, 5)) `shouldBe` true
      List.hasSubsequence(list, List(1)) `shouldBe` true
      List.hasSubsequence(list, List(2, 3)) `shouldBe` true
      List.hasSubsequence(list, List(2, 3, 4)) `shouldBe` true
      List.hasSubsequence(list, List(3, 4)) `shouldBe` true
      List.hasSubsequence(list, List(4, 5)) `shouldBe` true
      List.hasSubsequence(list, List(5)) `shouldBe` true
    }

    "リストの中にサブシーケンスがない場合はfalseになること" in {
      val list = List(1, 2, 3, 4, 5)
      List.hasSubsequence(list, List(0, 1)) `shouldBe` false
      List.hasSubsequence(list, List(5, 4, 3, 2, 1)) `shouldBe` false
    }
  }
end ListTest
