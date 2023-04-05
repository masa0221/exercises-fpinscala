package fpinscala.iomonads

import scala.collection.mutable.HashMap

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

    def runST[A](st: RunnableST[A]): A =
      st.apply[Unit].run(())._1
  }

  sealed trait STRef[S, A] {
    protected var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: A): ST[S, Unit] = new ST[S, Unit]:
      def run(s: S) = {
        cell = a
        ((), s)
      }
  }

  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
      var cell = a
    })
  }

  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  val p = new RunnableST[(Int, Int)] {
    def apply[S] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]
    def size: ST[S, Int] = ST(value.size)
    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }
    def read(i: Int): ST[S, A] = ST(value(i))
    def freeze: ST[S, List[A]] = ST(value.toList)

    def fill(xs: Map[Int, A]): ST[S, Unit] = xs.foldRight(ST[S, Unit](())) {
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }

    def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(
      new STArray[S, A] {
        lazy val value = xs.toArray
      }
    )

    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

  }

  object Immutable {
    // https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/localeffects/LocalEffects.scala#L109
    def partition[S](
        arr: STArray[S, Int],
        n: Int,
        r: Int,
        pivot: Int
    ): ST[S, Int] =
      for {
        vp <- arr.read(pivot)
        _ <- arr.swap(pivot, r)
        j <- STRef(n)
        _ <- (n until r).foldLeft(ST[S, Unit](()))((s, i) =>
          for {
            _ <- s
            vi <- arr.read(i)
            _ <-
              if vi < vp then
                for {
                  vj <- j.read
                  _ <- arr.swap(i, vj)
                  _ <- j.write(vj + i)
                } yield ()
              else ST[S, Unit](())
          } yield ()
        )
        x <- j.read
        _ <- arr.swap(x, r)
      } yield x

    def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] =
      if n < r then
        for
          pi <- partition(a, n, r, n + (r + n) / 2)
          _ <- qs(a, n, pi - 1)
          _ <- qs(a, pi + 1, r)
        yield ()
      else ST[S, Unit](())

    def quicksort(xs: List[Int]): List[Int] =
      if (xs.isEmpty) xs
      else
        ST.runST(new RunnableST[List[Int]] {
          def apply[S] = for {
            arr <- STArray.fromList(xs)
            size <- arr.size
            _ <- qs(arr, 0, size - 1)
            sorted <- arr.freeze
          } yield sorted
        })
  }

  object STArray {
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = Array.fill(sz)(v)
      })

    def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = xs.toArray
      })
  }

  sealed trait STMap[S, K, V]:
    protected def table: HashMap[K, V]
    def size(k: K): ST[S, Int] = ST(table.size)
    def apply(k: K): ST[S, V] = ST(table(k))
    def get(k: K): ST[S, Option[V]] = ST(table.get(k))
    def +=(kv: (K, V)): ST[S, Unit] = ST(table += kv)
    def -=(k: K): ST[S, Unit] = ST(table -= k)

  object STMap:
    def empty[S, K, V]: ST[S, STMap[S, K, V]] =
      ST(new STMap[S, K, V] {
        val table = HashMap.empty[K, V]
      })

    def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] =
      ST(new STMap[S, K, V] {
        val table = (HashMap.newBuilder ++= m).result()
      })
}
