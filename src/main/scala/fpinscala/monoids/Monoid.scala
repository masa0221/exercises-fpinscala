package fpinscala.monoids

import scala.Option
import fpinscala.answers.testing.exhaustive.Gen
import fpinscala.answers.testing.exhaustive.Prop

trait Monoid[A]:
  def op(a1: A, a2: A): A
  def zero: A

object Monoid:
  val stringMonoid = new Monoid[String]:
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""

  val intAddition: Monoid[Int] = new Monoid[Int]:
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  val intMultiplication: Monoid[Int] = new Monoid[Int]:
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 0

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean]:
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]:
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = false

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]:
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def zero = Option.empty[A]

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A]:
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero = m.zero

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid

  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]:
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero = (a: A) => a

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    import fpinscala.answers.testing.exhaustive.Prop.forAll
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)
    ) &&
    forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def flatMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldLeft[A, B](as: List[A])(z: B)(op: (B, A) => B): B =
    if (as.isEmpty) z else this.foldLeft(as.tail)(op(z, as.head))(op)
