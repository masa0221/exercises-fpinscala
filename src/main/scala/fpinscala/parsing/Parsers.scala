package fpinscala.parsing

import fpinscala.answers.testing.*

// 最小限の台数と関連する法則を設計について考える
// 追加したい解析タスクについて検討するためのお題
//
// 1. 'a'の文字を0個以上認識するParser[Int]
// "aa" の場合: 2を返す
// "b123" の場合: 0を返す
//
// 2. 'a'の文字を1個以上認識するParser[Int]
// "aa" の場合: 2を返す
// "b123" の場合: "Expected one or more 'a'" を明示
//
// 3. 0個以上の'a'に続いて1個以上の'b'を認識するパーサー
// "bbb"の場合: (0,3)
// "aaaab"の場合: (4,1)
//

// Parsers実装ルール
// 1. 主要な定義(String => Parser[String]など)は Parsers に直接配置する
// 2. 2項演算子やメソッドはParserOptに記述する
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
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  // 入力値の中に 「"ab" か "cad" どちらか」が3回ある
  // 成り立つ式
  // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  // run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  // run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // 1. 'a'の文字を0個以上認識するParser[Int]
  // "aa" の場合: 2を返す
  // "b123" の場合: 0を返す
  // map(many(char('a')))(_.size)
  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

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

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
