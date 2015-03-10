object Solution {

  def ways(m: Int, n: Int) = {
    val p = 1e9.toInt + 7 // this is a prime
    // the ways of matrix tracing is Combinations(m + n - 1, m - 1)
    val N = m + n - 2
    val K = m - 1
    val numerator = (N to N - K + 1 by -1).foldLeft(1L) { _ * _ % p }
    val denominator = (1 to K).foldLeft(1L) { _ * _ % p }
    // we can get inverse mod p, since p is a prime
    numerator * BigInt(denominator).modPow(p - 2, p) % p
  }
  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(m, n) = readLine.split(" ").map(_.toInt)
      println(ways(m, n))
    }
  }
}
