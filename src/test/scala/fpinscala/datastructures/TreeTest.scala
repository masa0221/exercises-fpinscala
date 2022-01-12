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

  "depth" - {
    "リストの最大の深さが取得できること" in {
      val leaf1 = Tree.Leaf(1) // 1
      val leaf2 = Tree.Leaf(2) // 1
      val leaf3 = Tree.Leaf(3) // 1
      val leaf4 = Tree.Leaf(4) // 1
      val branch1 = Tree.Branch(leaf1, leaf2) // 2
      val branch2 = Tree.Branch(branch1, leaf2) // 3
      val branch3 = Tree.Branch(leaf3, leaf4) // 2
      val branch4 = Tree.Branch(branch3, branch2) // 4
      branch1.depth `shouldBe` 2
      branch2.depth `shouldBe` 3
      branch4.depth `shouldBe` 4
    }
  }

  "map" - {
    "mapが機能すること" in {
      val leaf1 = Tree.Leaf(1)
      val leaf2 = Tree.Leaf(2)
      val leaf3 = Tree.Leaf(3)
      val branch1 = Tree.Branch(leaf1, leaf2)
      val branch2 = Tree.Branch(branch1, leaf3)
      branch2.map(_ * 10) `shouldBe` Tree.Branch(
        Tree.Branch(Tree.Leaf(10), Tree.Leaf(20)),
        Tree.Leaf(30)
      )
    }
  }

end TreeTest
