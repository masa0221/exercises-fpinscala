package fpinscala.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int =
    def go(t: Tree[A], acc: Int): Int = t match
      case Leaf(_)      => acc + 1
      case Branch(l, r) => go(l, acc + 1).max(go(r, acc + 1))
    go(this, 0)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int =
    fold(_ => 1, (l, r) => 1 + l + r)

  def depthViaFold: Int =
    fold(_ => 1, (l, r) => (l + 1).max(r + 1))

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)), (l, r) => Branch(l, r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = ???

  extension (t: Tree[Int])
    def maximum: Int =
      def go(t: Tree[Int]): Int = t match
        case Leaf(v)      => v
        case Branch(l, r) => go(l).max(go(r))
      go(t)

  extension (t: Tree[Int])
    def maximumViaFold: Int =
      t.fold(a => a, (l, r) => l.max(r))
