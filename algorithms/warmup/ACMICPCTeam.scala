object Solution {

  def main(args: Array[String]) {
    val Array(n, m) = readLine.split(" ").map(_.toInt)
    val nums = (1 to n).map(_ => BigInt(readLine, 2))
    val counts = for {
      i <- 0 until nums.size
      j <- i + 1 until nums.size
    } yield (nums(i) | nums(j)).bitCount

    val maxCount = counts.max
    val maxCountC = counts.filter(_ == maxCount).size
    println(maxCount)
    println(maxCountC)
  }
}
