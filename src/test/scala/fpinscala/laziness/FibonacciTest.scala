package fpinscala.laziness

// import fpinscala.laziness.Fibonacci.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class FibonacciTest extends AnyFreeSpecLike with Matchers:
  "fib" - {
    "フィボナッチ数列が5個取得できること" in {
      Fibonacci.fib().take(5).toList `shouldBe` List(0, 1, 1, 2, 3)
    }
    "フィボナッチ数列が10個取得できること" in {
      Fibonacci.fib().take(10).toList `shouldBe` List(0, 1, 1, 2, 3, 5, 8, 13,
        21, 34)
    }
  }
