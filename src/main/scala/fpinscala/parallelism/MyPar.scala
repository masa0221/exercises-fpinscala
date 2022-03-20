package fpinscala.parallelism

object MyPar:
  // def sum(ints: Seq[Int]): Int =
  //   ints.foldLeft(0)((a, b) => a + b)

  // IndexedSeq: Vector などのスーパークラス
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else
      // 半部に分けて計算させる
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)

end MyPar

object MyExamples:
  import MyPar.*
end MyExamples
