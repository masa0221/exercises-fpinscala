package fpinscala.iomonads

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def doWhile(cond: A => F[Boolean]): F[Unit] = for {
      al <- fa 
      ok <- cond(al)
      _ <- if (ok) doWhile(a)(cond) else unit(()) // unit?どこの?
    }

    def forever[B]: F[B] = {
      lazy val t: F[B] = forever(fa)
      fa flatMap (_ => t)
    }

    def foldM[B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[B] = l match
      case h #:: t => f(z,h) flatMap(z2 => foldM(t)(z2)(f))
      case _ => unit(z)

    def foldM_[B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] = 
      skip { foldM(l)(z)(f) }

    def foreachM(l: Stream[A])(f: A => F[Unit]): F[Unit] = 
      foldM_(l)(())((u,a) => skip(f(a)))
