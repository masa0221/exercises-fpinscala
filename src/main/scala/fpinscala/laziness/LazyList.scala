package fpinscala.laziness

enum LazyList[+A]:
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
    def go(acc: LazyList[A], cnt: Int): LazyList[A] = acc match
      case Empty => Empty
      case Cons(h, t) =>
        if (cnt <= 0) Empty else LazyList.cons(h(), go(t(), cnt - 1))
    go(this, n)

  def drop(n: Int): LazyList[A] =
    def go(l: LazyList[A], cnt: Int): LazyList[A] = l match
      case Empty => Empty
      case Cons(h, t) =>
        if (cnt <= 0) l else go(t(), cnt - 1)
    go(this, n)

  def takeWhile(p: A => Boolean): LazyList[A] =
    def go(l: LazyList[A]): LazyList[A] = l match
      case Empty => Empty
      case Cons(h, t) =>
        if (p(h())) LazyList.cons(h(), go(t())) else go(t())
    go(this)

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
    foldRight(LazyList.empty[B])((a, b) => LazyList.cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((h, t) =>
      if (f(h)) Cons(() => h, () => t.filter(f)) else t.filter(f)
    )

  def startsWith[B](s: LazyList[B]): Boolean = ???

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???
