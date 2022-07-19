package fpinscala.parsing

import language.higherKinds

// TODO: exercise 9.9 がわからなかったので以下を写経する
// https://github.com/fpinscala/fpinscala/blob/first-edition/answers/src/main/scala/fpinscala/parsing/JSON.scala
// https://github.com/fpinscala/fpinscala/blob/second-edition/src/main/scala/fpinscala/answers/parsing/JSON.scala
enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] =
    import P.*

    def token(s: String) = string(s).token

    // [] で囲まれた文字をarrayとする
    def array = ???

    // {} で囲まれた文字をobjectとする
    def obj = ???

    // "key": "value" の形?
    def keyval = ???

    // 実際の値(literalの略)
    def lit = ???

    def value: Parser[JSON] = ???

    ???
