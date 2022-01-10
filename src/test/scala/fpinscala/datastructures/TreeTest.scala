package fpinscala.datastructures

import fpinscala.datastructures.Tree
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class TreeTest extends AnyFreeSpecLike with Matchers:
  "size" - {
    "ノードを数えることができること" in {
      val leaf1 = Tree.Leaf(1)
      val leaf2 = Tree.Leaf(2)
      val leaf3 = Tree.Leaf(3)
      val branch = Tree.Branch(leaf1, leaf2)
      Tree.Branch(branch, leaf3).size `shouldBe` 5
    }
  }

  "maximum" - {
    "最大値を取得できること" in {
      val leaf1 = Tree.Leaf(1)
      val leaf2 = Tree.Leaf(2)
      val leaf3 = Tree.Leaf(3)
      val branch1 = Tree.Branch(leaf1, leaf2)
      val branch2 = Tree.Branch(leaf3, leaf2)
      Tree.Branch(branch1, leaf3).maximum `shouldBe` 3
      Tree.Branch(branch2, leaf1).maximum `shouldBe` 3
    }
  }

end TreeTest
