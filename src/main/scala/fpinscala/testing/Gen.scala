package fpinscala.testing

import fpinscala.state.State
import fpinscala.state.RNG

trait Prop:
  // self type annotation
  // @see http://www.ne.jp/asahi/hishidama/home/tech/scala/class.html#h_class.this
  // thisじゃだめなのか、ええやんthisで、って思っちゃう
  self =>

  // checkがEitherに変わったため一旦未実装に戻す
  def &&(that: Prop): Prop = ???
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

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

case class Gen[A](sample: State[RNG, A])

object Gen:
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    g1.boolean.flatMap(b => if (b) g2 else g1)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

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
