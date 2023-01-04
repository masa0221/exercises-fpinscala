package fpinscala
package applicative

import monads.Functor
import java.util.Date
import scala.util.Try
import scala.util.matching.Regex

trait Applicative[F[_]] extends Functor[F]:
  self =>

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

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(map2(fa, fb)((a, b) => f(a, b, _)))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] = apply(map3(fa, fb, fc)((a, b, c) => f(a, b, c, _)))(fd)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] = new:
    def unit[A](a: => A) = (self.unit(a), G.unit(a))
    override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
      (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))

  def compose[G[_]](G: Applicative[G]): Applicative[[X] =>> F[G[X]]] = new:
    def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] = ???
    def unit[A](a: => A) = self.unit(G.unit(a))
    override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C) =
      self.map2(fga, fgb)(G.map2(_, _)(f))

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ???

// TODO: 動くようにする
// object Applicative:
//   // https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/applicative/Applicative.scala#L204-L209
//   given eitherMonad[E]: Monad[Either[E, _]] with
//     def unit[A](a: => A): Either[E, A] = Right(a)
//     extension [A](eea: Either[E, A])
//       override def flatMap[B](f: A => Either[E, B]) = eea match
//         case Right(a) => f(a)
//         case Left(b)  => Left(b)

// これによって全てのモナドがアプリカティブファンクタであることがわかる
trait Monad[F[_]] extends Applicative[F]:
  // Monadの実装では、少なくともunitを実装し、flatMapかjoinのいずれかとmapを上書きしなければならない
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
    flatMap(f(a))(b => g(b))
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

// OptionアプリカティブとOptionモナド
val F: Applicative[Option] = ???

val depts: Map[String, String] = Map("Alice" -> "development")
val salaries: Map[String, Double] = Map("Alice" -> 100000.00)
val idsByName: Map[String, String] = Map("Alice" -> "Alice")

// Applicativeの場合
// プリミティブ
// - unit, map2
// - unit, apply
// - map2, map
//
// - 計算の構造が固定されており、作用を並べていくだけ
// - Applicativeの計算はコンテキストに依存しない
//
// 計算の構造が固定であると言える(flatMapが使えない制限が有利に働くと言える)
val oWithApplicative: Option[String] =
  F.map2(depts.get("Alice"), salaries.get("Alice"))((dept, salary) =>
    s"Alice in $dept makes $salary par year"
  )

// Monadの場合
// プリミティブ
// - unit, flatMap
// - unit, compose
// - unit, map, join
//
// - 前の計算の結果に基づいて構造を動的に選択できる
// - Monadの計算はコンテキストに依存させることができる
// - 作用はファーストクラス
//   - 「解釈」時に生成
//   - プログラムによって事前に選択されない
//
// ある検索結果を次の検索に反映させたい場合
val oWithMonad: Option[String] =
  // ApplicativeにflatMapを実装する手段はない(ので以下のことが発生しないと言える)
  // 前の計算の結果が次に実行される計算に影響を及ぼす可能性がある
  idsByName.get("Alice").flatMap { id =>
    // idsByNameの検索結果が次の結果に作用する
    F.map2(depts.get(id), salaries.get(id))((dept, salary) =>
      s"Alice in $dept makes $salary par year"
    )
  }

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Applicative {
  // REPLでの実行結果
  // scala> streamApplicative.sequence(List(streamApplicative.unit(1)))
  // val res3: Stream[List[Int]] = Stream(List(1), <not computed>)
  // scala> streamApplicative.sequence(List(streamApplicative.unit(1))).head
  // val res4: List[Int] = List(1)
  // scala> streamApplicative.sequence(List(streamApplicative.unit(1))).tail
  // val res5: Stream[List[Int]] = Stream(List(1), <not computed>)
  val streamApplicative = new Applicative[Stream] {

    def apply[A, B](fab: Stream[A => B])(fa: Stream[A]): Stream[B] = ???
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](
        a: Stream[A],
        b: Stream[B]
    )( // Combine elements pointwise
        f: (A, B) => C
    ): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]
      : Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def apply[A, B](fab: Validation[E, A => B])(
          fa: Validation[E, A]
      ): Validation[E, B] = ???
      def unit[A](a: => A) = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C
      ) = (fa, fb) match
        case (Success(a), Success(b))              => Success(f(a, b))
        case (Success(_), failure @ Failure(_, _)) => failure
        case (failure @ Failure(_, _), Success(_)) => failure
        case (Failure(ha, ta), Failure(hb, tb)) =>
          Failure(ha, ta ++ Vector(hb) ++ tb)
    }

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] = Try(
    (new java.text.SimpleDateFormat("yyyy-MM-dd")).parse(birthdate)
  ) match
    case scala.util.Success(v) => Success(v)
    case scala.util.Failure(_) =>
      Failure("Birthdate must be in the form yyyy-MM-dd")

  def validPhone(phoneNumber: String): Validation[String, String] =
    val numberPattern: Regex = "[0-9]{3,4}-?[0-9]{3,4}-?[0-9]{3,4}-?".r
    if (numberPattern.matches(phoneNumber)) Success(phoneNumber)
    else Failure("The specified phone number cannot be available format")

  def validWebForm(
      name: String,
      birthdate: String,
      phone: String
  ): Validation[String, WebForm] =
    validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone)
    )(
      WebForm(_, _, _)
    )

  val oF: Applicative[Option] = ???
  case class Employee(name: String, id: Int)
  case class Pay(rate: Double, hoursPerYear: Double)

  // def format(e: Option[Employee], pay: Option[Pay]): Option[String] =
  //   oF.map2(e, pay) { (e, pay) =>
  //     s"${e.name} makes ${pay.rate * pay.hoursPerYear}"
  //   }
  def format(e: Option[String], pay: Option[Double]): Option[String] =
    oF.map2(e, pay) { (e, pay) => s"${e} makes ${pay}" }

  val employee: Option[Employee] = ???
  val pay: Option[Pay] = ???

  format(
    oF.map(employee)(_.name),
    oF.map(pay)(p => p.rate * p.hoursPerYear)
  )
}
