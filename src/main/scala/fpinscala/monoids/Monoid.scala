package fpinscala.monoids

trait Monoid[A]:
  def op(a1: A, a2: A): A
  def zero: A

val stringMonad = new Monoid[String]:
  def op(a1: String, a2: String): String = a1 + a2
  def zero: String = ""

val intAddition: Monoid[Int] = ???
val intMultiplication: Monoid[Int] = ???
val booleanOr: Monoid[Boolean] = ???
val booleanAnd: Monoid[Boolean] = ???
