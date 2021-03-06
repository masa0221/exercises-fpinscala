package fpinscala.errorhandling

import fpinscala.errorhandling.Either.*
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class EitherTest extends AnyFreeSpecLike with Matchers:
  "map" - {
    "Leftの場合はmapは関数が適用されないこと" in {
      Left(10).map(_ => "type A,B not found") `shouldBe` Left(10)
    }
    "Rightの場合は関数が適用されること" in {
      Right(10).map(_ * 10) `shouldBe` Right(100)
    }
  }

  "flatMap" - {
    "Leftの場合は何も処理されないこと" in {
      Left("error").flatMap(a => Right("right!")) `shouldBe` Left("error")
    }
    "Rightの場合は関数が適用されること" in {
      Right(2).flatMap(a => Right(a * 100)) `shouldBe` Right(200)
    }
  }

  "orElse" - {
    "Leftの場合は引数の値が取得できること" in {
      Left("").orElse(Left("error")) `shouldBe` Left("error")
    }
    "Rightの場合はRightが持っている値が取得できること" in {
      Right(4).orElse(Left(5)) `shouldBe` Right(4)
    }
  }

  "map2" - {
    "Leftの場合はLeftを返すこと" in {
      Left("error").map2(Right(1))((a, b) => "failed") `shouldBe` Left("error")
    }
    "元の値がRightだとしてしてもMap2の内容がLeftの場合はLeftを返すこと" in {
      Right(1).map2(Left("error"))((a, b) => "failed") `shouldBe` Left("error")
    }
    "両方Rightの場合は関数が適用された結果のRightを返すこと" in {
      Right(1).map2(Right(2))((a, b) => a + b) `shouldBe` Right(3)
    }
  }

  "sequence" - {
    "リストの中にLeftがある場合は、最初のLeftの値を返すこと" in {
      Either.sequence(
        List(Right("ok"), Left("error1"), Left("error2"))
      ) `shouldBe` Left("error1")
    }
    "リストの中が全てRightの場合は、Right型になり、値はListになること" in {
      Either.sequence(
        List(Right("ok1"), Right("ok2"), Right("ok3"))
      ) `shouldBe` Right(List("ok1", "ok2", "ok3"))
    }
  }

  "traverse" - {
    "リストの値が全てRightの場合は結合されること" in {
      Either.traverse(List(1, 2, 3))(a => Right(a.toString)) `shouldBe` Right(
        List("1", "2", "3")
      )
    }
    "リストの中に一つでもLeftになる内容がある場合は結果がLeftになること" in {
      Either.traverse(List(0, 1, 2, 3))(a =>
        if (a == 0) Left("error") else Right(a.toString)
      ) `shouldBe` Left("error")
    }
  }

  "map2All" - {
    "1つ目だけLeftの場合は一つ目だけのエラーがリストで返されること" in {
      Either.map2All(
        Left(List("error1")),
        Right(1),
        (a, b) => "no implemnts"
      ) `shouldBe` Left(List("error1"))
    }
    "2つ目だけLeftの場合は一つ目だけのエラーがリストで返されること" in {
      Either.map2All(
        Right(1),
        Left(List("error2")),
        (a, b) => "no implemnts"
      ) `shouldBe` Left(List("error2"))
    }
    "両方の値がLeftの場合は両方のエラーがリストで返されること" in {
      Either.map2All(
        Left(List("error1")),
        Left(List("error2")),
        (a, b) => "no implemnts"
      ) `shouldBe` Left(List("error1", "error2"))
    }
    "両方Rightの場合は関数が適用された結果が返されること" in {
      Either.map2All(Right(1), Right(1), _ + _) `shouldBe` Right(2)
    }
  }

  "traverseAll" - {
    "リストの値が全てRightの場合は結合されること" in {
      Either.traverseAll(
        List(1, 2, 3),
        a => Right(a.toString)
      ) `shouldBe` Right(
        List("1", "2", "3")
      )
    }
    "リストの中にLeftになる内容がある場合は結果がLeftになること" in {
      Either.traverseAll(
        List(1, 2, 3),
        a =>
          if (a % 2 != 0) Left(List("error: value => " + a))
          else Right(a.toString)
      ) `shouldBe` Left(List("error: value => 1", "error: value => 3"))
    }
  }

  "sequenceAll" - {
    "リストの中にLeftがある場合は、Leftの値のリストを返すこと" in {
      Either.sequenceAll(
        List(Right("ok"), Left(List("error1")), Left(List("error2")))
      ) `shouldBe` Left(List("error1", "error2"))
    }
    "リストの中が全てRightの場合は、Right型になり、値はListになること" in {
      Either.sequenceAll(
        List(Right("ok1"), Right("ok2"), Right("ok3"))
      ) `shouldBe` Right(List("ok1", "ok2", "ok3"))
    }
  }

end EitherTest
