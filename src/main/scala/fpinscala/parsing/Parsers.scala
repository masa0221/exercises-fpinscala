package fpinscala.parsing

import fpinscala.answers.testing.*

// Parsers実装ルール
// 1. 主要な定義(String => Parser[String]など)は Parsers に直接配置する
// 2. 2項演算子やメソッドはParserOptに記述する
//
// 例: 0個以上のaに続いて1個以上のbを解析するためのパーサー
// char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
trait Parsers[ParseError, Parser[+_]] { self =>
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

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
    // product(a, b).map(t => f(t._1, t._2))
    product(a, b).map(f.tupled)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // run(slice(('a'|'b').many))("aaba") == Right("aaba")
  def slice[A](p: Parser[A]): Parser[String]

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
    def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
