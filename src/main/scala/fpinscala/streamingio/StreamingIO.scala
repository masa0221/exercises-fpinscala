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

  def dropWhile[I](f: I => Boolean): Process[I, I] = ???

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
