package fpinscala.datastructures

import fpinscala.datastructures.List
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class ListSuite extends AnyFreeSpecLike with Matchers:
  val listInt = List(1, 2, 3)
  val listDouble = List(1.0, 2.0, 3.0)
  val listString = List("one", "two", "three")

  "tailメソッドは先頭の要素を削除する" in {
    List.tail(listInt) `shouldBe` List(2, 3)
    List.tail(listDouble) `shouldBe` List(2.0, 3.0)
    List.tail(listString) `shouldBe` List("two", "three")
  }

end ListSuite
