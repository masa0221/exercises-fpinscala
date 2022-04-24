package fpinscala.testing

trait Prop:
  // self type annotation
  // @see http://www.ne.jp/asahi/hishidama/home/tech/scala/class.html#h_class.this
  // thisじゃだめなのか、ええやんthisで、って思っちゃう
  self =>

  def &&(that: Prop): Prop =
    new Prop:
      // that なら thisでええやないかと思っちゃうんだけど違うんか・・・郷にいればなんとやらか。
      def check = self.check && that.check

  def check: Boolean

object Prop:
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

trait Gen[A]:
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

