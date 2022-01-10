package fpinscala.datastructures

import fpinscala.datastructures.Tree
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class TreeTest extends AnyFreeSpecLike with Matchers:
  "size" - {
    "ノードを数えることができる" in {
      val leaf1 = Tree.Leaf(1)
      val leaf2 = Tree.Leaf(2)
      val leaf3 = Tree.Leaf(3)
      val branch = Tree.Branch(leaf1, leaf2)
      Tree.Branch(branch, leaf3).size `shouldBe` 5
    }
  }

end TreeTest
