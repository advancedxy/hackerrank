object Solution {
  val moduloP = 1e9.toInt + 7

  def diagonalSum(x: Long): Int = {
    val m = x / 2
    ((1 + BigInt(m) * BigInt(m + 1) * BigInt(x) * 8 / 3 + 2 * BigInt(m).pow(2) +
    6 * m) % moduloP).toInt
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toLong
      println(diagonalSum(n))
    }
  }
}
