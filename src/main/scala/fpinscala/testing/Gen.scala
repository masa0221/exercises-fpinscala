package fpinscala.testing

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

trait Gen[A]:
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
