package fpinscala.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    // Unreachable case except for null (if this is intentional, consider writing case null => instead).
    // case _ => 101
    case null => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      // case Nil => Nil
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => Cons(h, Nil)
      case x => Cons(h, x)

  def drop[A](l: List[A], n: Int): List[A] =
    def loop[A](l: List[A], n: Int): List[A] =
      l match
        case Nil => Nil
        case Cons(h, t) if n == 1 => t
        case Cons(h, t) => Cons(h, loop(t, n - 1))

    loop(l, n)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    def loop[A](l: List[A], f: A => Boolean): List[A] =
      l match
        case Nil => Nil
        case Cons(h, t) if f(h) => t
        case Cons(h, t) => Cons(h, loop(t, f))

    loop(l, f)


  def init[A](l: List[A]): List[A] =
    l match
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))


  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, xs) => 1 + xs)


  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]) =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    // Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    // foldLeft(Cons(2, Cons(3, Cons(4, Cons(5, Nil)))), Cons(1, List()))
    // foldLeft(Cons(3, Cons(4, Cons(5, Nil))), Cons(2, Cons(1, List())))
    // foldLeft(Cons(4, Cons(5, Nil)), Cons(3, Cons(2, Cons(1, List()))))
    // foldLeft(Cons(5, Nil), Cons(4, Cons(3, Cons(2, Cons(1, List())))))
    // foldLeft(Nil, Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, List()))))))
    foldLeft(l, List[A](), (acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def foldLeftViaFoldRight[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    // 要確認
    foldRight(l, (b:B) => b, (a, g) => b => g(f(b, a)))(acc)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (a, b) => Cons(a, b))

  def concat[A](l: List[List[A]]): List[A] = ???

  def incrementEach(l: List[Int]): List[Int] = ???

  def doubleToString(l: List[Double]): List[String] = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def filter[A](as: List[A], f: A => Boolean): List[A] = ???

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = ???

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = ???

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
