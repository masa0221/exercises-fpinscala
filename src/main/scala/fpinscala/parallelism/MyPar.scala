package fpinscala.parallelism

import java.util.concurrent.*

object MyPar:
  // opaque オリジナルのタイプを定義できるやーつ
  opaque type MyPar[A] = ExecutorService => Future[A]

  def unit[A](a: A): MyPar[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  extension [A](pa: MyPar[A])
    def map2Timeouts[B, C](pb: MyPar[B])(f: (A, B) => C): MyPar[C] =
      es =>
        new Future[C]:
          private val futureA = pa(es)
          private val futureB = pb(es)
          // @see http://www.ne.jp/asahi/hishidama/home/tech/java/thread.html#h2_volatile
          // "volatileを付けると、「あるスレッドで更新された値が別スレッドで読み込まれる」ことが保証される。"
          @volatile private var cache: Option[C] = None

          def isDone = cache.isDefined
          def get() = get(Long.MaxValue, TimeUnit.NANOSECONDS)

          def get(timeout: Long, units: TimeUnit) =
            val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
            val started = System.nanoTime
            val a = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
            val elapsed = System.nanoTime - started
            val b = futureB.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)
            val c = f(a, b)
            cache = Some(c)
            c

          def isCancelled = futureA.isCancelled || futureB.isCancelled
          def cancel(evenIfRunning: Boolean) =
            futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)

  def lazyUnit[A](a: => A): MyPar[A] = fork(unit(a))

  def map[A, B](a: MyPar[A])(f: A => B): MyPar[B] =
    map2(a, unit(()))((a, _) => f(a))

  def map2[A, B, C](a: MyPar[A], b: MyPar[B])(f: (A, B) => C): MyPar[C] =
    (es: ExecutorService) =>
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))

  def fork[A](a: => MyPar[A]): MyPar[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def run[A](s: ExecutorService)(a: MyPar[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => MyPar[B] = a => lazyUnit(f(a))

  def sortPar(parList: MyPar[List[Int]]): MyPar[List[Int]] =
    map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): MyPar[List[B]] = fork {
    val fbs: List[MyPar[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[MyPar[A]]): MyPar[List[A]] =
    ps.foldRight(unit(List()))((a, b) => map2(a, b)((aa, bb) => aa :: bb))

  def parFilter[A](as: List[A])(f: A => Boolean): MyPar[List[A]] =
    val ps: List[MyPar[List[A]]] =
      as.map(asyncF(a => if (f(a)) List(a) else List()))
    val sequencedPar: MyPar[List[List[A]]] = sequence(ps)
    // 中置記法でかけない？
    map(sequencedPar)(_.flatten)

  def equal[A](e: ExecutorService)(p: MyPar[A], p2: MyPar[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => MyPar[A]): MyPar[A] =
    es => fa(es)

  def choice[A](cond: MyPar[Boolean])(t: MyPar[A], f: MyPar[A]): MyPar[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

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
      // MyPar.map2(MyPar.fork(sum(l)), MyPar.fork(sum(r)))(_ + _)
      // MyPar.fork(sum(l)).map2Timeouts(MyPar.fork(sum(r)))(_ + _)
      MyPar.fork(sum(l)).map2Timeouts(sum(r))(_ + _)
end MyExamples
