package fpinscala.parsing.instances

import fpinscala.parsing.*
import scala.util.matching.Regex

// https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/parsing/instances/Reference.scala
object Reference extends Parsers[Parser] {
  type Parser[+A] = Location => Result[A]

  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)
  def attempt[A](p: Parser[A]): Parser[A] =
    l => p(l).uncommit

  def defer[A](p: => Parser[A]): Parser[A] = ???
  def errorLocation(
      e: ParseError
  ): Location = ???
  def errorMessage(e: ParseError): String = ???
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    l =>
      p(l) match
        case Success(a, n) =>
          f(a)(l.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case f @ Failure(_, _) => f

  def furthest[A](p: Parser[A]): Parser[A] = ???
  def latest[A](p: Parser[A]): Parser[A] = ???
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    l =>
      s1(l) match
        case Failure(e, false) => s2(l)
        case result            => result

  // https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/parsing/instances/Reference.scala#L64-L69
  def regex(r: Regex): Parser[String] =
    l =>
      r.findPrefixOf(l.remaining) match
        case None    => Failure(l.toError(s"regex $r"), true)
        case Some(m) => Success(m, m.length)

  def run[A](p: Parser[A])(
      input: String
  ): Either[ParseError, A] = ???

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.push(l, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.label(msg))

  // https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/parsing/instances/Reference.scala#L94-L97
  def slice[A](p: Parser[A]): Parser[String] =
    l =>
      p(l) match
        case Success(a, n)           => Success(l.slice(n), n)
        case failure @ Failure(_, _) => failure

  // https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/parsing/instances/Reference.scala#L45-L54
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int =
    var i = 0
    while (i + offset < s1.length && i < s2.length)
      if s1.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s1.length - offset >= s2.length then -1
    else s1.length - offset

  def string(w: String): Parser[String] =
    l =>
      val i = firstNonmatchingIndex(l.input, w, l.offset)
      if i == -1 then fpinscala.parsing.Success(w, w.length)
      else fpinscala.parsing.Failure(l.advanceBy(i).toError(s"$w"), true)

}
