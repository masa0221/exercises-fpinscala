package fpinscala.testing

import fpinscala.state.State
import fpinscala.state.RNG

trait Prop:
  // self type annotation
  // @see http://www.ne.jp/asahi/hishidama/home/tech/scala/class.html#h_class.this
  // thisじゃだめなのか、ええやんthisで、って思っちゃう
  self =>

  // checkがEitherに変わったため一旦未実装に戻す
  // new Prop:
  //   // that なら thisでええやないかと思っちゃうんだけど違うんか・・・郷にいればなんとやらか。
  //   def check = self.check && that.check

  import fpinscala.testing.Prop.*
  // 失敗の型を定義していないのは、
  // 失敗したケースでさらに計算を行う必要がないから
  // (失敗した情報の出力のみ行えば良いので)
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

object Prop:
  // 型エイリアスを設定するとコードが読みやすくなる
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  case class Prop(run: (MaxSize, TestCases, RNG) => Result):
    def &&(that: Prop): Prop = Prop { (max, n, rng) =>
      this.run(max, n, rng) match
        case Passed => that.run(max, n, rng)
        case res    => res
    }

    def ||(that: Prop): Prop = Prop { (max, n, rng) =>
      this.run(max, n, rng) match
        case Falsified(_, _) => that.run(max, n, rng)
        case res             => res
    }

  sealed trait Result:
    def isFalsified: Boolean

  case object Passed extends Result:
    def isFalsified = false

  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result:
    def isFalsified = true

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  // // @TODO 本に書いているこれの意味がわからん
  // def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
  //   forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesParSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map(p => Prop { (max, _, rng) => p.run(max, casesParSize, rng) })
          .toList
          .reduce(_ && _)
      prop.run(max, n, rng)

  }

case class Gen[A](sample: State[RNG, A])

object Gen:

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    g1.boolean.flatMap(b => if (b) g2 else g1)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(
      State(RNG.double).flatMap(d =>
        if (d < g1Threshold) g1._1.sample else g2._1.sample
      )
    )

  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/testing/05.answer.md
  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/testing/06.answer.md
  // TODO: 答えと違う（けど、実装したらコンパイルエラー出る）
  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(self.sample.flatMap(a => f(a).sample))

    def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      def go(acc: List[A], count: Int)(rng: RNG): (List[A], RNG) =
        if (count <= 0) {
          (acc, rng)
        } else {
          val (a, rng2) = self.sample.run(rng)
          go(a :: acc, count - 1)(rng2)
        }
      size.flatMap(n => Gen(State(go(List.empty[A], n))))

    def unsized: SGen[A] =
      _ => self

type SGen[A] = Int => Gen[A]
object SGen:
  extension [A](self: SGen[A])
    def flatMap[B](f: A => Gen[B]): SGen[B] =
      i => self(i).flatMap(f)

    def boolean: SGen[Boolean] =
      i => self(i).boolean

    def listOfN(size: Gen[Int]): SGen[List[A]] =
      i => self(i).listOfN(size)
