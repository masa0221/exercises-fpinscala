package fpinscala.parsing.instances

import fpinscala.parsing.*

// https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/parsing/instances/Reference.scala
object Reference extends Parsers[Parser] {
  type Parser[+A] = Location => Result[A]

  def attempt[A](p: Parser[A]): Parser[A] = ???
  def defer[A](p: => Parser[A]): Parser[A] = ???
  def errorLocation(
      e: ParseError
  ): Location = ???
  def errorMessage(e: ParseError): String = ???
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def furthest[A](p: Parser[A]): Parser[A] = ???
  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???
  def latest[A](p: Parser[A]): Parser[A] = ???
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???
  implicit def regex(r: scala.util.matching.Regex): Parser[String] = ???
  def run[A](p: Parser[A])(
      input: String
  ): Either[ParseError, A] = ???
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???
  def slice[A](p: Parser[A]): Parser[String] = ???

  def string(w: String): Parser[String] =
    l =>
      // https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/parsing/instances/Reference.scala#L45-L54
      val i = firstNonmatchingIndex(l.input, w, l.offset)
      if i == -1 then fpinscala.parsing.Success(w, w.length)
      // https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/parsing/Parsers.scala#L157
      else fpinscala.parsing.Failure(l.advanceBy(i).toError(s"$w"), i != 0)

}
