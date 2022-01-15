package fpinscala.errorhandling

import fpinscala.errorhandling.Either
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class EitherTest extends AnyFreeSpecLike with Matchers:
  "no implements" - {
    "test something" in {
      1 `shouldBe` 1
    }
  }

end EitherTest
