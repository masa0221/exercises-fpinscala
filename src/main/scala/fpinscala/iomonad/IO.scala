package fpinscala.iomonads

import fpinscala.iomonads.Monad
import scala.language.postfixOps
import io.StdIn.readLine
import parallelism

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

  type Async[A] = Free[Par, A]

  sealed trait Free[F[_], A]:
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](sub: Free[F, A], k: A => Free[F, B])
      extends Free[F, B]

  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] =
    new Monad[({ type f[a] = Free[F, a] })#f]:
      def unit[A](a: => A): Free[F, A] = Return(a)
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        fa flatMap f

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match
    case Return(a)  => a
    case Suspend(r) => r()
    case FlatMap(s, k) =>
      s match
        case Return(aa)  => runTrampoline(k(aa))
        case Suspend(rr) => runTrampoline(k(rr()))
        case FlatMap(ss, kk) =>
          runTrampoline(ss flatMap { ss => kk(ss) flatMap k })

  // https://github.com/fpinscala/fpinscala/blob/first-edition/answers/src/main/scala/fpinscala/iomonad/IO.scala#L397-L402
  def run[F[_], A](fa: Free[F, A])(implicit F: Monad[F]): F[A] = step(fa) match
    case Return(a)              => F.unit(a)
    case Suspend(r)             => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")

  def step[F[_], A](async: Free[F, A]): Free[F, A] = async match
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => async

  // def run[A](async: Async[A]): Par[A] = step(async) match
  //   case Return(a)  => Par.unit(a)
  //   case Suspend(r) => r
  //   case FlatMap(x, f) =>
  //     x match
  //       case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
  //       case _          => sys.error("Impossible step eliminates these cases")

  sealed trait Console[A]:
    def toPar: Par[A]
    def toThunk: () => A
    def toReader: ConsoleReader[A]

  case object ReadLine extends Console[Option[String]]:
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run

    def run: Option[String] =
      try Some(readLine())
      catch { case e: Exception => None }

  case class PrintLine(line: String) extends Console[Unit]:
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)

  object Console:
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))

    def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
      runFree[Console, ConsoleReader, A](io)(consoleToReader)

  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](f: Console[A]): ConsoleReader[A] = f.toReader
  }

  val f1: Free[Console, Option[String]] = for {
    _ <- Console.printLn("I can only intaract with the console.")
    ln <- Console.readLn
  } yield ln

  trait Translate[F[_], G[_]]:
    def apply[A](f: F[A]): G[A]

  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFuncion0 =
    new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar =
    new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit
      G: Monad[G]
  ): G[A] =
    step(free) match
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; step eliminates these cases")

  given Monad[Function0] with
    def unit[A](a: => A) = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) = () => f(a())()

  given Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A, B](a: Par[A])(f: A => Par[B]) = Par.fork {
      Par.flatMap(a)(f)
    }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFuncion0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] =
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG):
      def apply[A](a: F[A]): Free[G, A] = Suspend { fg(a) }

    runFree(f)(t)(freeMonad[G])

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline {
      translate(a)(new (Console ~> Function0) {
        def apply[A](c: Console[A]) = c.toThunk
      })
    }

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }

  object ConsoleReader {
    given Monad[ConsoleReader] with
      def unit[A](a: => A) = ConsoleReader(_ => a)
      def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) =
        ra flatMap f
  }
}
