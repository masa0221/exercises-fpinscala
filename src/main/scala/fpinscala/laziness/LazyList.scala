package fpinscala.laziness

enum LazyList[+A]:
  import LazyList.*

  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    // def go(acc: LazyList[A], cnt: Int): LazyList[A] = acc match
    //   case Empty => Empty
    //   case Cons(h, t) =>
    //     if (cnt <= 0) Empty else LazyList.cons(h(), go(t(), cnt - 1))
    // go(this, n)
    unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        if (n <= 0) None else Some(h(), t().take(n - 1))
    }

  def drop(n: Int): LazyList[A] =
    def go(l: LazyList[A], cnt: Int): LazyList[A] = l match
      case Empty => Empty
      case Cons(h, t) =>
        if (cnt <= 0) l else go(t(), cnt - 1)
    go(this, n)

  def takeWhile(p: A => Boolean): LazyList[A] =
    // def go(l: LazyList[A]): LazyList[A] = l match
    //   case Empty => Empty
    //   case Cons(h, t) =>
    //     if (p(h())) LazyList.cons(h(), go(t())) else go(t())
    // go(this)
    def f: LazyList[A] => Option[(A, LazyList[A])] = s =>
      s match {
        case Empty => None
        case Cons(h, t) =>
          if (p(h())) Some((h(), t().takeWhile(p))) else f(t())
      }
    unfold(this)(f)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if (p(a)) LazyList.cons(a, b) else b)

  // def headOption: Option[A] = this match
  //   case Empty      => None
  //   case Cons(h, t) => Some(h())
  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    // foldRight(LazyList.empty[B])((a, b) => LazyList.cons(f(a), b))
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((h, t) =>
      if (f(h)) Cons(() => h, () => t.filter(f)) else t.filter(f)
    )

  def append[B >: A](as: => LazyList[B]): LazyList[B] =
    foldRight(as)((h, t) => Cons(() => h, () => t))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: LazyList[B]): Boolean =
    this.zipWith(s, (a, b) => a == b).forAll(_ == true)

  def zipWith[B, C](that: LazyList[B], f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (_, Empty) => None
      case (Empty, _) => None
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(
          (
            f(h1(), h2()),
            (t1(), t2())
          )
        )
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) =>
        Some(
          (
            (Some(h()), None),
            (t(), Empty)
          )
        )
      case (Empty, Cons(h, t)) =>
        Some(
          (
            (None, Some(h())),
            (Empty, t())
          )
        )
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(
          (
            ((Some(h1()), Some(h2()))),
            (t1(), t2())
          )
        )
    }

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  // val ones: LazyList[Int] = LazyList.cons(1, ones)
  val ones: LazyList[Int] = unfold(1)(s => Some((1, s)))

  // def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))
  def continually[A](a: A): LazyList[A] = unfold(a)(s => Some((a, s)))

  // def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))
  def from(n: Int): LazyList[Int] = unfold(n)(s => Some((s, s + 1)))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None    => LazyList.empty[A]
      case Some(v) => LazyList.cons(v._1, unfold(v._2)(f))
