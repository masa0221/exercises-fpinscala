package fpinscala.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = ???

  def map[B](f: A => B): Tree[B] = ???

  def fold[B](f: A => B, g: (B, B) => B): B = ???

  def sizeViaFold: Int = ???

  def depthViaFold: Int = ???

  def mapViaFold[B](f: A => B): Tree[B] = ???

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = ???

  extension (t: Tree[Int])
    def maximum: Int =
      def go(t: Tree[Int], acc: Int): Int = t match
        case Leaf(v)      => acc.max(v)
        case Branch(l, r) => go(l, acc).max(go(r, acc))
      go(t, 0)

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
