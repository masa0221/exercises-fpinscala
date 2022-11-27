trait Applicative[F[_]] extends Functor[F]:
  // プリミティブコンビネータ
  def map2[A,B,C](fa: F[A], fb: [B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // 派生コンビネータ
  def map[A,B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

