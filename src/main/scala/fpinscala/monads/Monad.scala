package fpinscala.monads

import fpinscala.answers.testing.exhaustive.Gen
import fpinscala.answers.parallelism.Nonblocking.Par
import fpinscala.answers.state.State

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

  // https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/monads/08.answer.scala
  // def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())
  //
  // https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/monads/13.answer.scala
  // def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
  //   join(map(ma)(f))
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, acc) => map2(ma, acc)(_ :: _))

  // https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/monads/03.answer.scala#L4-L5
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, lmb) => map2(f(a), lmb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    List
      .fill(n)(ma)
      .foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_ :: _))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/monads/06.answer.scala
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match
    case Nil => unit(Nil)
    case h :: t =>
      flatMap(f(h))(b =>
        if (!b)
          filterM(t)(f)
        else
          map(filterM(t)(f))(h :: _)
      )

  // def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
  //   flatMap(f(a))(g)
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
    join(map(f(a))(g))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

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

  val listMonad = new Monad[List]:
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f

// https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/monads/02.answer.scala
// 以下だと動かない
// val stateMonad = new Monad[State]:
//   def unit[S, A](a: => A): State[S, A] = State.unit(a)
//   def flatMap[S, A, B](ma: State[S, A])(f: A => State[S, B]) = ma flatMap f
class StateMonads[S]:
  type StateS[A] = State[S, A]

  val monad = new Monad[StateS]:
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(
        f: A => State[S, B]
    ): State[S, B] = st flatMap f

// https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/monads/17.answer.scala
case class Id[A](value: A):
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

object Id:
  val IdMonad = new Monad[Id]:
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f

type IntState[A] = State[Int, A]
object IntStateMonad extends Monad[IntState]:
  def unit[A](a: => A): IntState[A] = State(s => (a, s))
  def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] = st flatMap f
