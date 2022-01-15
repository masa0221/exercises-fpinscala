package fpinscala.errorhandling

import fpinscala.errorhandling.Option
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class OptionTest extends AnyFreeSpecLike with Matchers:
  "map" - {
    "Noneの場合はNone" in {
      None.map(a => a) `shouldBe` None
    }
    "Someの場合は関数が適応される" in {
      Some(10).map(_ * 10) `shouldBe` Some(100)
    }
  }

end OptionTest
