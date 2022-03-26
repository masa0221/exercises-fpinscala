package fpinscala.parallelism

case class MyPar[A](value: () => A)

object MyPar:
  def unit[A](a: A): MyPar[A] = MyPar(() => a)

  def lazyUnit[A](a: => A): MyPar[A] = fork(unit(a))

  def map2[A, B, C](a: MyPar[A], b: MyPar[B])(f: (A, B) => C): MyPar[C] =
    lazyUnit(f(run(a), run(b)))

  def fork[A](a: => MyPar[A]): MyPar[A] =
    // TODO: forkの実装がわからん
    lazy val value = a
    MyPar(() => run(value))

  def run[A](a: MyPar[A]): A = a.value()

end MyPar

object MyExamples:
  import MyPar.*
  // def sum(ints: Seq[Int]): Int =
  //   ints.foldLeft(0)((a, b) => a + b)

  // IndexedSeq: Vector などのスーパークラス
  def sum(ints: IndexedSeq[Int]): MyPar[Int] =
    if (ints.size <= 1) MyPar.unit(ints.headOption.getOrElse(0))
    else
      // 半部に分けて計算させる
      val (l, r) = ints.splitAt(ints.length / 2)
      // val sumL: MyPar[Int] = MyPar.unit(sum(l))
      // val sumR: MyPar[Int] = MyPar.unit(sum(r))
      // MyPar.get(sumL) + MyPar.get(sumR)
      MyPar.map2(MyPar.fork(sum(l)), MyPar.fork(sum(r)))(_ + _)
end MyExamples
