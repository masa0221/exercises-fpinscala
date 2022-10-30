package fpinscala.monads

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def condistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))

val listFunctor = new Functor[List]:
  def map[A, B](as: List[A])(f: A => B): List[B] = as map f

trait Mon[F[_]]:
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
