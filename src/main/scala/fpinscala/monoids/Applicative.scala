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

// val streamApplicative = new Applicative[Stream]:
//   def unit[A](a: => A): Stream[A] = Stream.continually(a)
//   override def map2[A, B, C](a: Stream[A], b: Stream[B])(
//       f: (A, B) => C
//   ): Stream[C] =
//     a zip b map f.tupled
