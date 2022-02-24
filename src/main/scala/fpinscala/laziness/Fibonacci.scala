package fpinscala.laziness

import scala.collection.immutable.Stream.*

object Fibonacci:
  def fib(): Stream[Int] =
    // def go(n1: Int, n2: Int): Stream[Int] =
    //   cons(n1, go(n2, n1 + n2))
    // go(0, 1)
    unfold((0, 1)) { case (n1, n2) =>
      Some((n1, (n2, n1 + n2)))
    }
