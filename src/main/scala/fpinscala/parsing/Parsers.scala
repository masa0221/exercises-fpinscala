package fpinscala.parsing

trait Parsers[ParseError, Parser[+_]] {
  // 成り立つ式
  // run(char(c))(c.toString) == Right(c)
  // run(string(s))(s) == Right(c)
  // run(or(string("hoge"), string("fuga")))("hoge") == Right("hoge")
  // run(or(string("hoge"), string("fuga")))("fuga") == Right("fuga")
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // 任意の一文字を認識
  def char(c: Char): Parser[Char]

  // 任意の文字列を認識
  def string(s: String): Parser[String]

  // 二つの文字列のどちらかをの文字列を認識
  // def orString(s1: String, s2: String): Parser[String]
  // ↓多相にしてみる
  def or(s1: Parser[String], s2: Parser[String]): Parser[String]
}
