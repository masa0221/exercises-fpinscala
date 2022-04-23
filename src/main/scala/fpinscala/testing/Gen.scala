package fpinscala.testing

trait Prop:
  def &&(p: Prop): Prop

object Prop:
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

trait Gen[A]:
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

