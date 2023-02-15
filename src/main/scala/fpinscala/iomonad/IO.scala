package fpinscala.iomonads

import fpinscala.iomonads.Monad
import scala.language.postfixOps
import io.StdIn.readLine

object IOSample1 {
  case class Player(name: String, score: Int)
  sealed trait IO[A] { self =>
    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)
  }

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  // def ++(io: IO): IO = new:
  //   def run = { self.run; io.run }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a) // syntax for IO { .. }

    def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO { value = a; a }
      def get: IO[A] = IO { value }
      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }
  }

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match
        case Return(a)     => run(f(a))
        case Suspend(r)    => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p map { case Player(name, _) =>
    s"$name is the winner!"
  } getOrElse "It's a draw."

  def ReadLine: IO[String] = IO { readLine() }

  // def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
  def PrintLine(msg: String): IO[Unit] = Suspend(() => println(msg))
  val p = IO.forever(PrintLine("Still going..."))

  // def contest(p1: Player, p2: Player): IO =
  //   PrintLine(winnerMsg(winner(p1, p2)))

  def fahrrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: Unit = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrrenheitToCelsius(d).toString)
  } yield ()

  def factorial(n: Int): IO[Int] = for {
    acc <- IO.ref(1)
    // IO[Int]型はMonadのskipを使えない?
    // acc.modify(_ * i).skip の書き方はダメなのか？
    _ <- IO.foreachM(1 to n toStream)(i => IO.skip(acc.modify(_ * i)))
    result <- acc.get
  } yield result

  val helpstring = """
  | The Amazing Factorial REPL, v2.0
  | q - quit
  | <number> - compute the factorial of the given number
  | <anything else> - bomb with horrible error
  """.trim.stripMargin

  val factorialREPL: IO[Unit] = IO.sequence_(
    IO { println(helpstring) },
    IO.doWhile { IO { readLine } } { line =>
      val ok = line != "q"
      IO.when(ok) {
        for {
          n <- factorial(line.toInt)
          _ <- IO { println("factorial: " + n) }
        } yield ()
      }
    }
  )
}

object IOSample2 {
  sealed trait TailRec[A]:
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B])
      extends TailRec[B]

}

object IOSample3 {
  import fpinscala.answers.parallelism.*

  sealed trait Async[A]:
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))

  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]
}
