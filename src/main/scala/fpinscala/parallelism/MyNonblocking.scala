package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch

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

  object MyPar:
  end MyPar
end MyNonblocking
