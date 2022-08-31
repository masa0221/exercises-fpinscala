package fpinscala.monoids

trait Monoid[A]:
  def op(a1: A, a2: A): A
  def zero: A

val stringMonad = new Monoid[String]:
  def op(a1: String, a2: String): String = a1 + a2
  def zero: String = ""

val intAddition: Monoid[Int] = new Monoid[Int]:
  def op(a1: Int, a2: Int): Int = a1 + a2
  def zero: Int = 0

val intMultiplication: Monoid[Int] = new Monoid[Int]:
  def op(a1: Int, a2: Int): Int = a1 * a2
  def zero: Int = 0

val booleanOr: Monoid[Boolean] = ???
val booleanAnd: Monoid[Boolean] = ???
