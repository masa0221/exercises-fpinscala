package fpinscala.parsing

import language.higherKinds

// TODO: exercise 9.2 がわからなかったので以下を写経する
// JSON.scalaは9.2の答えじゃなかった。。(後で使うから残しておこう)
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
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = ???
// import P.*
// val space = char(' ').many.slice

// // token???
// def token(s: String) = string(s).token
