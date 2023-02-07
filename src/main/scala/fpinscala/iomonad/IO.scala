package fpinscala.iomonads

import fpinscala.iomonads.Monad
import scala.language.postfixOps

case class Player(name: String, score: Int)
sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] =
    new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] { def run = f(self.run).run }
}

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

def winner(p1: Player, p2: Player): Option[Player] =
  if (p1.score > p2.score) Some(p1)
  else if (p1.score < p2.score) Some(p2)
  else None

def winnerMsg(p: Option[Player]): String = p map { case Player(name, _) =>
  s"$name is the winner!"
} getOrElse "It's a draw."

def ReadLine: IO[String] = IO { io.StdIn.readLine() }

def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

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

// val factorialREPL: IO[Unit] = sequence_(
//   IO { println(helpstring) },
//   doWhile { IO { readLine } } { line =>
//     val ok = line != "q"
//     when(ok) {
//       for {
//         n <- factorial(line.toInt)
//         _ <- IO { println("factorial: " + n) }
//       } yield ()
//     }
//   }
// )
