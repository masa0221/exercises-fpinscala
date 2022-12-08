package fpinscala
package applicative

import monads.Functor

trait Applicative[F[_]] extends Functor[F]:
  // プリミティブコンビネータ
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  // map2をプリミティブにした場合
  // def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  // def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

  // 派生コンビネータ
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ???

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = ???

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))
