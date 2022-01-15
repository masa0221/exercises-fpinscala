package fpinscala.errorhandling

import fpinscala.errorhandling.Option
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class OptionTest extends AnyFreeSpecLike with Matchers:
  "no implements" - {
    "test something" in {
      1 `shouldBe` 1
    }
  }

end OptionTest
