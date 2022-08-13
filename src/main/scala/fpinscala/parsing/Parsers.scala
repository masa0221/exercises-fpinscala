package fpinscala.parsing

import fpinscala.answers.testing.*
import scala.util.matching.Regex

// Parsers実装ルール
// 1. 主要な定義(String => Parser[String]など)は Parsers に直接配置する
// 2. 2項演算子やメソッドはParserOptに記述する
//
// 例: 0個以上のaに続いて1個以上のbを解析するためのパーサー
// char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // run(scceed(a))(s) == Right(s)
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  // 任意の一文字を認識
  // 成り立つ式
  // run(char(c))(c.toString) == Right(c)
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  // 任意の文字列を認識
  // def string(s: String): Parser[String]
  // 成り立つ式
  // run(string(s))(s) == Right(c)
  implicit def string(s: String): Parser[String]

  // 二つの文字列のどちらかをの文字列を認識
  // def orString(s1: String, s2: String): Parser[String]
  // ↓多相にしてみる
  // def or(s1: Parser[String], s2: Parser[String]): Parser[String]
  // 成り立つ式
  // run(or(string("hoge"), string("fuga")))("hoge") == Right("hoge")
  // run(or(string("hoge"), string("fuga")))("fuga") == Right("fuga")
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // 入力値の中に 「"ab" か "cad" どちらか」が3回ある
  // 成り立つ式
  // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  // run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  // run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
  // https://github.com/fpinscala/fpinscala/blob/first-edition/answerkey/parsing/04.answer.scala
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/parsing/05.answer.md
  def defer[A](p: => Parser[A]): Parser[A]

  // 1. 'a'の文字を0個以上認識するParser[Int]
  // "aa" の場合: 2を返す
  // "b123" の場合: 0を返す
  // map(many(char('a')))(_.size)
  // https://github.com/fpinscala/fpinscala/blob/first-edition/answers/src/main/scala/fpinscala/parsing/Parsers.scala#L42-L43
  def many[A](p: Parser[A]): Parser[List[A]] =
    // map2(p, many(p))(_ :: _) | succeed(List())
    map2(p, defer(many(p)))(_ :: _) | succeed(List())

  // 'a'の文字を1個以上認識する
  // https://github.com/fpinscala/fpinscala/blob/first-edition/answers/src/main/scala/fpinscala/parsing/Parsers.scala#L35-L36
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.flatMap(b => succeed((a, b))))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(
      f: (A, B) => C
  ): Parser[C] =
    // product(p1, p2).map(t => f(t._1, t._2))
    // product(p1, p2).map(f.tupled)
    p1.flatMap(a => p2.flatMap(b => succeed(f(a, b))))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // run(slice(('a'|'b').many))("aaba") == Right("aaba")
  // char('a').many.slice // Parser[List[Char]]
  def slice[A](p: Parser[A]): Parser[String]

  // run(regex("[a-z0-9]*".r))("abc123efg") == Right("abc123efg")
  implicit def regex(r: Regex): Parser[String]

  def letter: Parser[String] = regex("[a-zA-Z]+".r)
  def digit: Parser[String] = regex("[0-9]+".r)
  def whitespace: Parser[String] = regex("\\s*".r)

  // 解析へのコミットを先送りさせる
  // attempt が満たす条件
  // attempt(p flatMap (_ => fail)) or p2 == p2
  //
  // "abra cadabra!" を解析する場合の例
  // (attempt("abra" ** spaces ** "abra") ** "cadabra") or ("abra" ** space "cadabra!")
  // ("abra" ** space "cadabra!") がコミットされる
  def attempt[A](p: Parser[A]): Parser[A]

  // 繰り返すが、whitespaceは無視する
  def token[A](p: Parser[A]): Parser[A] = p.attempt <* whitespace

  // p2の方は無視する
  def <*[A](p1: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p1, p2.slice)((a, _) => a)

  // p1の方は無視する
  def *>[A, B](p1: Parser[A], p2: => Parser[B]): Parser[B] =
    map2(p1.slice, p2)((_, b) => b)

  // parseした内容を全てbにする
  // Parser[A] -> Parser[List[A]] -> Parser[List[B]]
  def as[A, B](p: Parser[A], b: B): Parser[B] = p.slice.map(_ => b)

  def sep[A](p1: Parser[A], separator: Parser[Any]): Parser[List[A]] =
    sep1(p1, separator) | succeed(List.empty[A])

  def sep1[A](p1: Parser[A], separator: Parser[Any]): Parser[List[A]] =
    p1.map2((separator *> p1).many)(_ :: _)

  def root[A](p: Parser[A]): Parser[A] = p <* eof

  def eof: Parser[String] = regex("\\z".r)

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/parsing/11.answer.md
  // エラーが発生した場合、最も多くの文字を消費した後に発生したエラーを返す
  def furthest[A](p: Parser[A]): Parser[A]

  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/parsing/11.answer.md
  // エラーが発生した場合、直近に発生したエラーを返す
  def latest[A](p: Parser[A]): Parser[A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  // def string と def asStringParser によって Stringが自動的にParserに昇格される
  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]):
    // "abc" | "def" と書いたとき
    // 1. | メソッドは Parser]A] | Parser[A] という書き方をする必要がある
    // 2. "abc" が String なので implicit で定義されたstringメソッドが実行される（ String => Parser[String] ）
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      self.map2(p, p2)(f)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def slice[B]: Parser[String] = self.slice(p)
    def <*(p2: => Parser[Any]): Parser[A] = self.<*(p, p2)
    def *>[B](p2: => Parser[B]): Parser[B] = self.*>(p, p2)
    def attempt: Parser[A] = self.attempt(p)
    def token: Parser[A] = self.token(p)
    def as[B](b: B): Parser[B] = self.as(p, b)
    def many: Parser[List[A]] = self.many(p)
    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p, separator)
    def root: Parser[A] = self.root(p)
    def furthest: Parser[A] = self.furthest(p)
    def latest: Parser[A] = self.latest(p)

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      Prop.forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _       => true
        }
      }
  }

}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match
    case -1        => offset + 1
    case lineStart => offset - lineStart

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))
}

case class ParseError(stack: List[(Location, String)])
