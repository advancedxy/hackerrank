object Solution {
  import math.BigInt

  def computeCombins(m: Int, n: Int): Int = {
    val p = 1e9.toInt + 7
    val ones = n - 1
    val zeros = m
    val onesWithP = (ones + 1 to ones + zeros).foldLeft(1l) { _ * _ % p }
    val zerosWithP = (1 to zeros).foldLeft(1L) { _ * _ % p }
    (onesWithP * BigInt(zerosWithP).modPow(p - 2, p) % p).toInt
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val Array(m, n) = readLine.split(" ").map(_.toInt)
      println(computeCombins(m, n))
    }
  }
}
