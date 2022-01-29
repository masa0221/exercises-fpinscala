package fpinscala
package applicative

import monads.Functor
import state.*
import State.*
import StateUtil.* // defined at bottom of this file
import monoids.*
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ???

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = ???

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = ???

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = ???

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???

  def product[G[_]](G: Applicative[G]): Applicative[[X] =>> F[G[X]]] = ???

  def compose[G[_]](G: Applicative[G]): Applicative[[X] =>> F[G[X]]] = ???

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ???
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[Either[E, *]] = ???

  def stateMonad[S] = new Monad[State[S, *]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st.flatMap(f)
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[[X] =>> F[N[X]]] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val lazyListApplicative = new Applicative[LazyList] {

    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: LazyList[A], b: LazyList[B])( // Combine elements pointwise
                    f: (A,B) => C): LazyList[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[Validation[E, *]] = ???

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[Const[M, *]] =
    new Applicative[Const[M, *]] {
      def unit[A](a: => A): M = M.empty
      override def apply[A,B](m1: M)(m2: M): M = M.combine(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative.*

  extension [A](fa: F[A])
    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      traverse[Const[B, *], A, Nothing](fa)(f)(using monoidApplicative(mb))

    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      ???

    override def toList: List[A] =
      ???

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, *], A, B](fa)(f)(using Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[[X] =>> F[G[X]]] = ???
}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
