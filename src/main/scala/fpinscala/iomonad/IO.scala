package fpinscala.iomonads

import fpinscala.monads.Monad

case class Player(name: String, score: Int)
sealed trait IO[A]:
  self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B]:
    def run = f(self.run)
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B]:
    def run = f(self.run).run

// def ++(io: IO): IO = new:
//   def run = { self.run; io.run }

object IO extends Monad[IO]:
  // def empty: IO = new IO { def run = () }
  def unit[A](a: => A): IO[A] = new IO[A]:
    def run = a
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

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
  acc <- ref(1)
  _ <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
  result <- acc.get
} yield result

val factorialREPL: IO[Unit] = sequence_(
  IO { println(helpstring) },
  doWhile { IO { readLine } } { line =>
    val ok = line != "q"
    when(ok) {
      for {
        n <- factorial(line.toInt)
        _ <- IO { println("factorial: " + n) }
      } yield ()
    }
  }
)

def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
  al <- a
  ok <- cond(al)
  _ <- if (ok) doWhile(a)(cond) else unit(()) // unit?どこの?
}

def forever[A, B](a: F[A]): F[B] = ???
def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[B] = ???
def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] = ???
def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] = ???
