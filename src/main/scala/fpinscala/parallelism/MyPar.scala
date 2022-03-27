package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import scala.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

case class MyPar[A](value: () => A)

object MyPar:
  type MyPar[A] = ExecutorService => UnitFuture[A]

  case class UnitFuture[A](value: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = value
    def isCaancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): MyPar[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): MyPar[A] = fork(unit(a))

  def map2[A, B, C](a: MyPar[A], b: MyPar[B])(f: (A, B) => C): MyPar[C] =
    (es: ExecutorService) =>
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(1L, TimeUnit.SECONDS), bf.get(1L, TimeUnit.SECONDS)))
      // lazyUnit(f(run(a), run(b)))

  def fork[A](a: => MyPar[A]): MyPar[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get(1, TimeUnit.SECONDS)
    })

  def run[A](s: ExecutorService)(a: MyPar[A]): Future[A] = a(s)

end MyPar

object MyExamples:
  import MyPar.*
  // def sum(ints: Seq[Int]): Int =
  //   ints.foldLeft(0)((a, b) => a + b)

  // IndexedSeq: Vector などのスーパークラス
  def sum(ints: IndexedSeq[Int]): MyPar[Int] =
    if (ints.size <= 1) MyPar.unit(ints.headOption.getOrElse(0))
    else
      // 半部に分けて計算させる
      val (l, r) = ints.splitAt(ints.length / 2)
      // val sumL: MyPar[Int] = MyPar.unit(sum(l))
      // val sumR: MyPar[Int] = MyPar.unit(sum(r))
      // MyPar.get(sumL) + MyPar.get(sumR)
      MyPar.map2(MyPar.fork(sum(l)), MyPar.fork(sum(r)))(_ + _)
end MyExamples
