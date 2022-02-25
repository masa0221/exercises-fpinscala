package fpinscala.laziness

import fpinscala.state.State.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class StateTest extends AnyFreeSpecLike with Matchers:
  "method name" - {
    "no implements" in {
      true `shouldBe` true
    }
  }
end StateTest

