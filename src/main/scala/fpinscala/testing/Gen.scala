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

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/testing/05.answer.md
  // TODO: 答えと違う（けど、実装したらコンパイルエラー出る）
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    def go(acc: List[A], count: Int)(rng: RNG): (List[A], RNG) =
      if (count <= 0) {
        (acc, rng)
      } else {
        val (a, rng2) = g.sample.run(rng)
        go(a :: acc, count - 1)(rng2)
      }
    Gen(State(go(List.empty[A], n)))
