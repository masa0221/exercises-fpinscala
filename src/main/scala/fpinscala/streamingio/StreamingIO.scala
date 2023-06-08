package fpinscala.streamingio

import fpinscala.iomonads.IOSample1.IO

object StreamingIO0 {
  def lineGt40k(filename: String): IO[Boolean] = IO {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines()
      while (count <= 4000 && lines.hasNext) {
        lines.next
        count += 1
      }
      count > 4000
    } finally src.close
  }

  def lines(filename: String): IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream append { src.close; Stream.empty }
  }
}

object SimpleStreamTransducers:
  object Process:
    case class Emit[I, O](
        head: O,
        tail: Process[I, O] = Halt[I, O]()
    ) extends Process[I, O]

    case class Await[I, O](
        recv: Option[I] => Process[I, O]
    ) extends Process[I, O]

    case class Halt[I, O]() extends Process[I, O]

    def liftOne[I, O](f: I => O): Process[I, O] = Await {
      case None        => Halt()
      case Some(value) => Emit(f(value))
    }

    def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

    def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] =
        Await {
          case Some(d) => Emit(d + acc, go(d + acc))
          case None    => Halt()
        }

      go(0.0)
    }

    def sumWithLoop: Process[Double, Double] = {
      loop(0.0)((d: Double, acc: Double) => (d + acc, d + acc))
    }

    def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
      Emit(head, tail)

    def await[I, O](
        f: I => Process[I, O],
        fallback: Process[I, O] = Halt[I, O]()
    ): Process[I, O] =
      Await[I, O] {
        case Some(i) => f(i)
        case None    => fallback
      }

    def take[I](n: Int): Process[I, I] =
      if (n <= 0) Halt()
      else await(i => emit(i, take[I](n - 1)))

    def drop[I](n: Int): Process[I, I] =
      if (n <= 0) id
      else await(i => emit(i, drop[I](n - 1)))

    def id[I]: Process[I, I] = lift(identity)

    def takeWhile[I](f: I => Boolean): Process[I, I] =
      await(i => if (f(i)) emit(i, takeWhile[I](f)) else Halt())

    def dropWhile[I](f: I => Boolean): Process[I, I] =
      await(i => if (f(i)) dropWhile[I](f) else emit(i, id))

    def count[I]: Process[I, Int] = {
      def go(n: Int): Process[I, Int] =
        await((i: I) => emit(n + 1), go(n + 1))
      go(0)
    }

    def countWithLoop[I]: Process[I, Int] = {
      loop(0)((_: I, n) => (n + 1, n + 1))
    }

    def mean: Process[Double, Double] = {
      def go(sum: Double, n: Double): Process[Double, Double] =
        await((d: Double) => emit((sum + d) / (n + 1), go(sum + d, n + 1)))
      go(0, 0)
    }

    def mean2: Process[Double, Double] = (sum zip count) |> lift {
      case (s, d) =>
        s / d
    }

    def exists[I](f: I => Boolean): Process[I, Boolean] = lift(f) |> any

    def any: Process[Boolean, Boolean] = loop(false)((i, s) => (i || s, i || s))

    def existsResult[I](f: I => Boolean) =
      exists(f) |> takeThrough(!_) |> dropWhile(!_) |> echo.orElse(emit(false))

    def takeThrough[I](f: I => Boolean): Process[I, I] = takeWhile(f) ++ echo

    def echo[I]: Process[I, I] = await(i => emit(i))

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await((i: I) =>
        f(i, z) match
          case (o, s2) => emit(o, loop(s2)(f))
      )

    def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] =
      (p1, p2) match
        case (Halt(), _)                => Halt()
        case (_, Halt())                => Halt()
        case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
        case (Await(recv1), _) =>
          Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
        case (_, Await(recv2)) =>
          Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))

    def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] =
      p match
        case Halt()      => p
        case Emit(h, t)  => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa)

    def processFile[A, B](f: java.io.File, p: Process[String, A], z: B)(
        g: (B, A) => B
    ): IO[B] = IO {
      def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
        cur match
          case Emit(head, tail) => go(ss, tail, g(acc, head))
          case Await(recv) =>
            val next = if (ss.hasNext) recv(Some(ss.next())) else recv(None)
            go(ss, next, acc)
          case Halt() => acc

      val s = io.Source.fromFile(f)
      try go(s.getLines, p, z)
      finally s.close
    }

  sealed trait Process[I, O]:
    import Process.*

    def apply(s: Stream[I]): Stream[O] = this match
      case Halt() => Stream()
      case Await(recv) =>
        s match
          case h #:: t => recv(Some(h))(t)
          case xs      => recv(None)(xs)
      case Emit(h, t) => h #:: t(s)

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match
        case Emit(head, tail) => Emit(head, go(tail))
        case Await(recv) =>
          Await {
            case None  => recv(None)
            case value => go(recv(value))
          }
        case Halt() => go(this)

      go(this)
    }

    def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match
      case Halt() => Halt()
      case Await(f) =>
        this match
          case Halt()           => Halt() |> f(None)
          case Await(g)         => Await((i: Option[I]) => g(i) |> p2)
          case Emit(head, tail) => tail |> f(Some(head))
      case Emit(head, tail) => Emit(head, this |> tail)

    def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

    def ++(p: Process[I, O]): Process[I, O] = this match
      case Halt()      => p
      case Emit(h, t)  => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match
      case Halt()      => Halt()
      case Emit(h, t)  => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))

    import fpinscala.iomonads.Monad
    def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] =
      new Monad[({ type f[x] = Process[I, x] })#f] {
        def unit[O](o: => O): Process[I, O] = emit(o)
        def flatMap[O, O2](p: Process[I, O])(
            f: O => Process[I, O2]
        ): Process[I, O2] = p flatMap f
      }

    def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] =
      Process.zip(this, p)

    def zipWithIndex: Process[I, (O, Int)] =
      this zip (count map (_ - 1))

    def orElse(p: Process[I, O]): Process[I, O] = this match
      case Halt() => p
      case Await(recv) =>
        Await {
          case None => p
          case x    => recv(x)
        }
      case _ => this

    def convertFahrenheit: Process[String, String] =
      filter((line: String) => !line.startsWith("#")) |>
        filter(line => line.trim.nonEmpty) |>
        lift(line => toCelsius(line.toDouble).toString)

    def toCelsius(fahrenheit: Double): Double =
      (5.0 / 9.0) * (fahrenheit - 32.0)

// https://github.com/fpinscala/fpinscala/blob/first-edition/answers/src/main/scala/fpinscala/streamingio/StreamingIO.scala#L514
object GeneralizedStreamTransducers:
  trait Process[F[_], O]:
    import Process._
    def map[O2](f: O => O2): Process[F, O2] = this match
      case Await(req, recv) => Await(req, recv andThen (_ map f))
      case Emit(h, t)       => Try { Emit(f(h), t map f) }
      case Halt(err)        => Halt(err)

    def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match
      case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
      case Emit(h, t)       => Try(f(h)) ++ t.flatMap(f)
      case Halt(err)        => Halt(err)

    def repeat: Process[F, O] = this ++ this.repeat

    // onHaltを使用
    def ++(p: Process[F, O]): Process[F, O] = this.onHalt {
      case End => Try(p)
      case err => Halt(err)
    }

    def |>[O2](p2: Process1[O, O2]): Process[F, O2] = ???

    def filter(f: O => Boolean): Process[F, O] = this |> Process.filter(f)

    def take(n: Int): Process[F, O] = this |> Process.take(n)

    def once: Process[F, O] = take(1)

    def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match
      case Halt(e)          => Try(f(e))
      case Emit(h, t)       => Emit(h, t.onHalt(f))
      case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))

    def onComplete(p: => Process[F, O]): Process[F, O] = ???

  object Process:
    case class Await[F[_], A, O](
        req: F[A],
        recv: Either[Throwable, A] => Process[F, O]
    ) extends Process[F, O]

    case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

    case class Halt[F[_], O](err: Throwable) extends Process[F, O]

    case object End extends Exception
    case object Kill extends Exception

    def emit[F[_], O](
        head: O,
        tail: Process[F, O] = Halt[F, O](End)
    ): Process[F, O] = Emit(head, tail)

    def await[F[_], A, O](
        req: F[A],
        recv: Either[Throwable, A] => Process[F, O]
    ): Process[F, O] = Await(req, recv)

    def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
      try p
      catch { case e: Throwable => Halt(e) }

    case class Is[I]() {
      sealed trait f[X]
      val Get = new f[I] {}
    }
    def Get[I] = Is[I]().Get

    type Process1[I, O] = Process[Is[I]#f, O]

    def await1[I, O](
        recv: I => Process1[I, O],
        fallback: => Process1[I, O] = halt1[I, O]
    ): Process1[I, O] = Await(
      Get[I],
      (e: Either[Throwable, I]) =>
        e match
          case Left(End) => fallback
          case Left(err) => Halt(err)
          case Right(i)  => Try(recv(i))
    )

    def emit1[I, O](h: O, tl: Process1[I, O] = halt1[I, O]): Process1[I, O] =
      emit(h, tl)

    def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

    def filter[I](f: I => Boolean): Process1[I, I] =
      await1[I, I](i => if (f(i)) emit(i) else halt1).repeat

    def take[I](n: Int): Process1[I, I] =
      if (n < 0) halt1
      else await1(i => emit(i, take(n - 1)))

    def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
      val E = java.util.concurrent.Executors.newFixedThreadPool(4)
      // @annotation.tailrec
      def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] = cur match
        case Emit(head, tail) => go(tail, acc :+ head)
        case Await(req, recv) => ???
        // val next =
        //   // unsafePerformIOが、、
        //   try recv(Right(fpinscala.iomonads.unsafePerformIO(req)(E)))
        //   catch { case err: Throwable => recv(Left(err)) }
        // go(next)
        case Halt(End) => acc
        case Halt(err) => throw err

      try go(src, IndexedSeq())
      finally E.shutdown
    }
