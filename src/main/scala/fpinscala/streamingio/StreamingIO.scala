import fpinscala.iomonads.IOSample1.IO

object StreamingIO {
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

sealed trait Process[I, O]

case class Emit[I, O](
    head: O,
    tail: Process[I, O] = Halt[I, O]()
) extends Process[I, O]

case class Await[I, O](
    recv: Option[I] => Process[I, O]
) extends Process[I, O]

class Halt[I, O]() extends Process[I, O]
