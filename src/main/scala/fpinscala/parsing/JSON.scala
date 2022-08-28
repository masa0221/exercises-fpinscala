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
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*

    def token(s: String): Parser[String] = string(s).token

    // [] で囲まれた文字をarrayとする
    def array: Parser[JSON] = token("[") *> value
      .sep(token(","))
      .map(values => JArray(values.toIndexedSeq)) <* token("]")

    // {} で囲まれた文字をobjectとする
    def obj: Parser[JSON] =
      token("{") *> keyval
        .sep(token(","))
        .map(kv => JObject(kv.toMap)) <* token("}")

    // "key": "value" の形
    def keyval: Parser[(String, JSON)] = letter ** (token(":") *> value)

    // 実際の値
    def literal: Parser[JSON] =
      token("true").as(JBool(true)) |
        token("false").as(JBool(false)) |
        token("null").as(JNull) |
        digit.map(n => JNumber(n.toDouble)) |
        letter.map(s => JString(s))

    def value: Parser[JSON] = literal | array | obj

    (whitespace *> (obj | array)).root
