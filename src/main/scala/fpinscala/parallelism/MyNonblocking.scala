package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable

object MyNonblocking:
  trait Future[A]:
    private[parallelism] def apply(k: A => Unit): Unit

  opaque type MyPar[+A] = ExecutorService => Future[A]

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

  object MyPar:
  end MyPar
end MyNonblocking
