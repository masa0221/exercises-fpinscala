package fpinscala.monoids

import scala.Option
import fpinscala.answers.testing.exhaustive.Gen
import fpinscala.answers.testing.exhaustive.Prop
import fpinscala.answers.parallelism.Nonblocking._

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

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)]:
      def op(ma2: (A, B), mb2: (A, B)): (A, B) =
        (ma.op(ma2._1, mb2._1), mb.op(ma2._2, mb2._2))
      def zero: (A, B) = (ma.zero, mb.zero)

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]]:
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, v.op(a.getOrElse(k, v.zero), b.getOrElse(k, v.zero)))
        }

  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B]:
    def zero: A => B = a => m.zero
    def op(fa: A => B, fb: A => B): A => B = a => m.op(fa(a), fb(a))

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
    // if (as.isEmpty) z else this.foldLeft(as.tail)(op(z, as.head))(op)
    // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/monoids/06.answer.md
    flatMap(as, dual(endoMonoid))(a => b => op(b, a))(z)

  def foldRight[A, B](as: List[A])(z: B)(op: (A, B) => B): B =
    // op(a, (z) => )
    // as match
    //   case Nil    => z
    //   case h :: t => this.foldRight(t)(op(h, z))(op)
    // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/monoids/06.answer.md
    flatMap(as, endoMonoid)(op.curried)(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 1) f(v.head)
    else if (v.length == 0) m.zero
    else
      // scala> IndexedSeq(1,2,3).splitAt(3 / 2)
      // val res1: (IndexedSeq[Int], IndexedSeq[Int]) = (Vector(1),Vector(2, 3))
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    def op(a1: Par[A], a2: Par[A]): Par[A] =
      a1.flatMap(aa1 => a2.map(aa2 => m.op(aa1, aa2)))
    def zero = Par.unit(m.zero)

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.unit(f(a)))

  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/monoids/09.answer.md
  opaque type Interval = (Int, Int)

  val orderdMonoid: Monoid[(Boolean, Option[Interval])] = new:
    def op(
        a1: (Boolean, Option[Interval]),
        a2: (Boolean, Option[Interval])
    ): (Boolean, Option[Interval]) = (a1._2, a2._2) match
      case (Some((leftMin, leftMax)), Some((rightMin, rightMax))) =>
        (a1._1 && a2._1 && leftMax <= rightMin, Some((leftMin, rightMax)))
      case _ => (a1._1 && a2._1, a1._2.orElse(a2._2))
    def zero: (Boolean, Option[Interval]) = (true, None)

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderdMonoid)(i => (true, Some(i, i)))(0)

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new:
    def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(lStub, words, rStub)) =>
        Part(a + lStub, words, rStub)
      case (Part(lStub, words, rStub), Stub(b)) =>
        Part(lStub, words, rStub + b)
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
        val words = if ((rStub1 + lStub2).isEmpty) 0 else 1
        Part(lStub1 + lStub2, words1 + words + words2, rStub1 + rStub2)
    def zero: WC = Stub("")

  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/monoids/11.answer.md
  def count(strings: String): Int =
    def wc(c: Char): WC =
      if c.isWhitespace then Part("", 0, "") else Stub(c.toString)

    def unstub(s: String): Int = if s.isEmpty then 0 else 1

    foldMapV(strings, wcMonoid)(wc) match
      case Stub(s)       => unstub(s)
      case Part(l, n, r) => unstub(l) + n + unstub(r)

// https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/monoids/12.answer.scala
trait Foldable[F[_]]:
  import fpinscala.monoids.Monoid.*

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => b => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty[A])(_ :: _)

// TODO: geven instance
// https://docs.scala-lang.org/scala3/reference/contextual/givens.html
object FoldableList extends Foldable[List]:
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))

object FoldableIndexedSeq extends Foldable[IndexedSeq]:
  import fpinscala.monoids.Monoid.*

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)

object FoldableStream extends Foldable[Stream]:
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object FoldableTree extends Foldable[Tree]:
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match
      case Leaf(a)      => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r, z, f))(f)

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match
      case Leaf(a)      => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l, z, f))(f)

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match
      case Leaf(a)      => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))

object FoldableOption extends Foldable[Option]:
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match
      case Some(a) => f(a, z)
      case None    => z

  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match
      case Some(a) => f(z, a)
      case None    => z

  def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match
      case Some(a) => mb.op(f(a), mb.zero)
      case None    => mb.zero
