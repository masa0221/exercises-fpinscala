package fpinscala.streamingio

import fpinscala.iomonads._

trait MonadCatch[F[_]] extends Monad[F]:
  def attempt[A](a: F[A]): F[Either[Throwable, A]]
  def fail[A](t: Throwable): F[A]

object MonadCatch
