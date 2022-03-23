package fpinscala.parallelism

case class MyPar[A](value: () => A)

object MyPar:
  def unit[A](a: => A): MyPar[A] =
    lazy val value = a
    MyPar(() => value)

  def get[A](a: MyPar[A]): A = a.value()

  def map2[A, B, C](a: MyPar[A], b: MyPar[B])(f: (A, B) => C): MyPar[C] =
    unit(f(get(a), get(b)))

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
      MyPar.map2(sum(l), sum(r))(_ + _)
end MyExamples
