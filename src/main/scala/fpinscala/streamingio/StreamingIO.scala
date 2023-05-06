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

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    await((i: I) =>
      f(i, z) match
        case (o, s2) => emit(o, loop(s2)(f))
    )

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
    ???

  def zipWithIndex: Process[I, (O, Int)] =
    this zip (count map (_ - 1))
