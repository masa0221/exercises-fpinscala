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

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    // scala> Int.MaxValue
    // val res0: Int = 2147483647
    // scala> Int.MinValue
    // val res1: Int = -2147483648
    // 0 ~ 2147483647 を返すようにするには-2147483648の値だけ絶対値にしても対応できない
    val (n, rng2) = rng.nextInt
    val res = if (n < 0) (n + 1) * -1 else n
    (res, rng2)

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

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (in, rng2) = nonNegativeInt(rng)
    val (dn, rng3) = double(rng2)
    ((in, dn), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = ???

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      ???

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
