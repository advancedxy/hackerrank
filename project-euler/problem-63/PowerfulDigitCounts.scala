object Solution {

  def main(args: Array[String]) {
    val n = readLine.toInt
    val start = math.pow(10, n - 1)
    val end = start * 10
    val intStart = math.pow(start, 1d / n).toLong - 1
    val intEnd = math.pow(end, 1d / n).toLong + 1
    for (i <- intStart until intEnd) {
      val num = BigInt(i).pow(n)
      if (num >= BigInt(10).pow(n - 1) && num < BigInt(10).pow(n)) println(num)
    }
  }
}
