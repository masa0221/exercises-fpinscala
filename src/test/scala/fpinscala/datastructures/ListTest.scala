package fpinscala.datastructures

import fpinscala.datastructures.List
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class ListSuite extends AnyFreeSpecLike with Matchers:
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
      List.setHead(listDouble, 0) `shouldBe` List(0, 1.0 ,2.0, 3.0)
      List.setHead(listString, "zero") `shouldBe` List("zero", "one", "two", "three")
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
      List.dropWhile(listString, v => v == "three") `shouldBe` List("one", "two")
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

end ListSuite
