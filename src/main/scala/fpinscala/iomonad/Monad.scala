package fpinscala.iomonads

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]

  def map[A, B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))

  def as[A, B](a: F[A])(b: B): F[B] = map(a)(_ => b)
  def skip[A](a: F[A]): F[Unit] = as(a)(())

  def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[B] =
    l match {
      case h #:: t => flatMap(f(z, h))(z2 => foldM(t)(z2)(f))
      case _       => unit(z)
    }

  def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] =
    skip { foldM(l)(z)(f) }

  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((u, a) => skip(f(a)))

  def sequence_[A](fs: Stream[F[A]]): F[Unit] = foreachM(fs)(skip)

  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a // value flatMap is not a member of F[A]
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()
}
