object Solution {
  def finalNumbers(n: Int, acc: Set[Int], diffs: Set[Int]): Set[Int] =
    if (n == 0) acc
    else finalNumbers(n - 1, acc.flatMap(x => diffs.map(_ + x)), diffs)

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val (n, a, b) = (readLine.toInt, readLine.toInt, readLine.toInt)
      println(finalNumbers(n - 1, Set(0), Set(a, b)).toList.sorted.mkString(" "))
    }
  }
}
