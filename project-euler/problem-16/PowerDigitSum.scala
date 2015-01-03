object Solution {
  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      val sum = BigInt(2).pow(n).toString.map(_.asDigit).sum
      println(sum)
    }
  }
}
