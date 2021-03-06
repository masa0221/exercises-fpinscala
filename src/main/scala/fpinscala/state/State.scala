package fpinscala.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    // scala> Int.MaxValue
    // val res0: Int = 2147483647
    // scala> Int.MinValue
    // val res1: Int = -2147483648
    // 0 ~ 2147483647 を返すようにするには-2147483648の値だけ絶対値にしても対応できない
    val (n, rng2) = rng.nextInt
    val res = if (n < 0) (n + 1) * -1 else n
    (res, rng2)

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match
      case (i, rng2) => (i % 2 == 0, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = nonNegativeInt(rng)
    // こんなことをすると出力までにめっちゃ時間かかる
    // val dn = n.toDouble
    // if (dn >= 0 && dn < 1) (dn, rng2) else double(rng2)
    //
    // 模範解答
    // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/state/02.answer.md
    // scala> Int.MaxValue / Int.MaxValue.toDouble
    // val res3: Double = 1.0
    // scala> Int.MaxValue / (Int.MaxValue.toDouble + 1)
    // val res4: Double = 0.9999999995343387
    // +1 しているのは上記のようにInt.MaxValueが出た時の対処(1.0を含めたくないため)
    ((n / (Int.MaxValue.toDouble + 1)), rng2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (in, rng2) = nonNegativeInt(rng)
    val (dn, rng3) = double(rng2)
    ((in, dn), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (dn, rng2) = double(rng)
    val (in, rng3) = nonNegativeInt(rng)
    ((dn, in), rng3)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count > 0) {
      val (n, rng2) = nonNegativeInt(rng)
      val (l, rng3) = ints(count - 1)(rng2)
      (n :: l, rng3)
    } else {
      (Nil, rng)
    }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(nonNegativeInt))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    // RNG => (A, RNG)
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
    }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    // わいの実装・・・
    // def go(rs: List[Rand[A]], rng: RNG): (List[A], RNG) =
    //   rs match
    //     case Nil => (Nil, rng)
    //     case h :: t => {
    //       val (a, rng2) = h(rng)
    //       val (as, rng3) = go(t, rng2)
    //       (a :: as, rng3)
    //     }
    // rng => {
    //   go(rs, rng)
    // }
    // 答えみた実装
    // TODO: 見直し
    // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/state/07.answer.md
    rs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = r(rng)
      f(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.flatMap(b => unit(f(a, b))))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (a, s2) = run(s)
        f(a)(s2)
      }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  import Input.*
  import State.*
  // https://github.com/fpinscala/fpinscala/blob/second-edition/answerkey/state/11.answer.md

  def update = (i: Input) =>
    (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _))        => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _))  => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    // _ <- sequence(inputs.map(update).map(modify[Machine]))
    // _ <- sequence(inputs.map(modify[Machine] compose update))
    // andThenの方が直感的に見やすい
    _ <- sequence(inputs.map(update andThen modify[Machine]))
    s <- get
  } yield (s.coins, s.candies)
