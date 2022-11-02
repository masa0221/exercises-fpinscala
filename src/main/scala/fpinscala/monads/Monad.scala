package fpinscala.monads

import fpinscala.answers.testing.exhaustive.Gen
import fpinscala.answers.parallelism.Nonblocking.Par

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

trait Monad[F[_]]:
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

object Monad:
  val genMonad = new Monad[Gen]:
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f

  val parMonad = new Monad[Par]:
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = ma flatMap f

  val optionMonad = new Monad[Option]:
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f

  val streamMonad = new Monad[Stream]:
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
