object Solution {

  def main(args: Array[String]) {
    val n = readLine.toInt
    val nums = readLine.split(" ").map(_.toInt)
    val sortedNums = nums.sorted
    val diffs = (1 until n).map(i => math.abs(sortedNums(i) - sortedNums(i - 1)))
    val minDiff = diffs.min
    val pairs = (0 until n - 1).flatMap { i =>
      if (diffs(i) == minDiff) Some(sortedNums(i) + " " + sortedNums(i + 1))
      else None
    }
    println(pairs.mkString(" "))
  }
}
