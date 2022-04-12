package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable

object MyNonblocking:
  trait Future[A]:
    private[parallelism] def apply(k: A => Unit): Unit

  opaque type MyPar[+A] = ExecutorService => Future[A]

  object MyPar:
    def run[A](es: ExecutorService)(p: MyPar[A]): A =
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a =>
        ref.set(a); latch.countDown
      }
      latch.await
      ref.get

    def unit[A](a: A): MyPar[A] =
      es =>
        new Future[A]:
          def apply(cb: A => Unit): Unit = cb(a)

    def lazyUnit[A](a: => A): MyPar[A] = fork(unit(a))

    def fork[A](a: => MyPar[A]): MyPar[A] =
      es =>
        new Future[A]:
          def apply(cb: A => Unit): Unit =
            // a: () => MyPar[A]
            // () => ExecutorService => Future[A].apply(A => Unit)
            eval(es)(a(es)(cb))

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(
        new Callable[Unit]:
          def call = r
      )

    def map2[A, B, C](p: MyPar[A], p2: MyPar[B])(f: (A, B) => C): MyPar[C] =
      es =>
        new Future[C]:
          def apply(cb: C => Unit): Unit =
            var ar: Option[A] = None
            var br: Option[B] = None
            val combinater = Actor[Either[A, B]](es) {
              case Left(a) =>
                br match
                  case None    => ar = Some(a)
                  case Some(b) => eval(es)(cb(f(a, b)))
              case Right(b) =>
                ar match
                  case None    => br = Some(b)
                  case Some(a) => eval(es)(cb(f(a, b)))
            }
            p(es)(a => combinater ! Left(a))
            p2(es)(b => combinater ! Right(b))

    def parMap[A, B](ps: List[A])(f: A => B): MyPar[List[B]] = fork {
      val fbs: List[MyPar[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def asyncF[A, B](f: A => B): A => MyPar[B] = a => lazyUnit(f(a))

    def sequence[A](ps: List[MyPar[A]]): MyPar[List[A]] =
      ps.foldRight(unit(List.empty[A]))((a, b) =>
        map2(a, b)((aa, bb) => aa :: bb)
      )

  end MyPar
end MyNonblocking
