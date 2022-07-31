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

    def token(s: String): Parser[String] = string(s).token

    // [] で囲まれた文字をarrayとする
    def array = token("[") *> value.sep(token(",") <* token("]"))

    // {} で囲まれた文字をobjectとする
    def obj = token("{") *> keyval.sep(token(",") <* token("}"))

    // "key": "value" の形
    def keyval: Parser[(String, JSON)] = letter ** (token(":") *> value)

    // 実際の値
    def literal = (
      token("true").as(JBool(true)) |
        token("false").as(JBool(false)) |
        token("null").as(JNull) |
        digit.as(JNumber) |
        letter.as(JString)
    )

    def value: Parser[JSON] = ???

    ???
