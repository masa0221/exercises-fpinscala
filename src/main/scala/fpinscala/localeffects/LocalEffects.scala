package fpinscala.iomonads

object LocalEffects {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs
  else {
    val arr = xs.toArray

    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    def partition(n: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = n
      for (i <- n until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }

    def qs(n: Int, r: Int): Unit = if (n < r) {
      val pi = partition(n, r, n + (r - n) / 2)
      qs(n, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }

  sealed trait ST[S, A] { self =>
    protected def run(s: S): (A, S)
    def map[B](f: A => B): ST[S, B] = new ST[S, B]:
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }

    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B]:
      def run(s: S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
  }

  object ST {
    def apply[S, A](a: => A) = {
      lazy val memo = a
      new ST[S, A]:
        def run(s: S) = (memo, s)
    }
  }
}
