package fpinscala.iomonads

case class Player(name: String, score: Int)
trait IO { def run: Unit }

def winner(p1: Player, p2: Player): Option[Player] =
  if (p1.score > p2.score) Some(p1)
  else if (p1.score < p2.score) Some(p2)
  else None

def winnerMsg(p: Option[Player]): String = p map { case Player(name, _) =>
  s"$name is the winner!"
} getOrElse "It's a draw."

def PrintLine(msg: String): IO =
  new IO { def run = println(msg) }

def contest(p1: Player, p2: Player): IO =
  PrintLine(winnerMsg(winner(p1, p2)))
