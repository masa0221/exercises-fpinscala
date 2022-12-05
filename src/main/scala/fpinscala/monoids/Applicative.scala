package fpinscala
package applicative

import monads.Functor

trait Applicative[F[_]] extends Functor[F]:
  // プリミティブコンビネータ
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  // 派生コンビネータ
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried), fb)

  // def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[A]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))
