object Solution {
  import scala.annotation.tailrec

  def combinationWithRepetitions[T](ls: List[T], n: Int): Iterator[List[T]] = {
    val lset = ls.distinct
    if (n == 0) Iterator(List())
    else {
      for {
        x <- lset.toIterator
        xs <- combinationWithRepetitions(ls, n - 1)
      } yield x :: xs
    }
  }

  def combinationWithRepetitionsTail[T](ls: List[T],
                                        n: Int): Stream[List[T]] = {
    val lset = ls.distinct.toStream

    @tailrec
    def combinationAux(acc: Stream[List[T]], n: Int): Stream[List[T]]= {
      if (n == 0) acc
      else {
        val newAcc = lset.map(x => acc.map(x :: _)).flatten
        combinationAux(newAcc, n - 1)
      }
    }

    combinationAux(Stream(List()), n)
  }
  val numberDigits = (0 to 1e5.toInt).map(x => x.toString.map(_.asDigit).toList)
  val digitNumbers = numberDigits.zipWithIndex.toMap
  def genCombin(n: Int, d: Int, digits: List[Int]): List[(Int, Int)] = {
    val nDigits = numberDigits(n)
    val dDigits = numberDigits(d)

    def combineHelper[T](as: List[T],
                         bs: List[T],
                         acc: List[List[T]]): List[List[T]] = (as, bs) match {
      case (Nil, Nil) => acc
      case (head :: tail, Nil) => combineHelper(tail, Nil, acc.map(head :: _))
      case (Nil, head :: tail) => combineHelper(Nil, tail, acc.map(head :: _))
      case (ah :: at, bh :: bt) =>
        val newAcc = acc.map(ah :: bh :: _) ++ acc.map(bh :: ah :: _)
        combineHelper(at, bs, acc.map(ah :: _)) ++
        combineHelper(as, bt, acc.map(bh :: _))
    }

    val nNums = combineHelper(nDigits, digits, List(List()))
                .map(x => if (x.length == 1) digitNumbers(x) else digitNumbers(x.dropWhile(_ == 0)))
    val dNums = combineHelper(dDigits, digits, List(List()))
                .map(x => if (x.length == 1) digitNumbers(x) else digitNumbers(x.dropWhile(_ == 0)))
    for {
      n <- nNums
      d <- dNums
    } yield (n, d)
  }

  def genFractions(n: Int, k: Int): Seq[(Int, Int)] = {
    val m = n - k // the number of digits after cancelling
    val start = math.pow(10, m - 1).toInt
    val end = start * 10
    val fractions = for {
      denominator <- start + 1 until end
      numerator <- start until denominator
      cancelledDigits <- combinationWithRepetitionsTail((1 to 9).toList, k)
      (realN, realD) <- genCombin(numerator, denominator,
        cancelledDigits)
      if numberDigits(realN).size == n && numberDigits(realD).size == n &&
        denominator * realN == numerator * realD
      } yield {
      (realN, realD)
    }
    fractions
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val fractions = genFractions(n, k)
    val (numerator, denominator) = fractions.foldLeft((0, 0)) { (x, y) =>
      (x._1 + y._1, x._2 + y._2)
    }

    println(numerator + " " + denominator)
  }
}

