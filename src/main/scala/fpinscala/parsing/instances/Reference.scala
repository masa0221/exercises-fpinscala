package fpinscala.parsing.instances

import fpinscala.parsing.*

class Parser[+A]

object Reference extends Parsers[Parser] {
  type Parser[+A] = String => Either[ParseError, A]

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
  implicit def string(s: String): Parser[String] =
    (input: String) =>
      if (input.startsWith(s))
        Right(s)
      else
        Left(Location(input).toError("Expected: " + s))
}
