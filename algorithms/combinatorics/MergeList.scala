object Solution {

  def main(args: Array[String]) {
    val p = 1e9.toInt + 7
    for (_ <- 1 to readInt) {
      val Array(m, n) = readLine.split(" ").map(_.toInt)
      val numerator = (n + 1 to m + n).foldLeft(1L) { _ * _ % p }
      val denominator = (1 to m).foldLeft(1L) { _ * _ % p }
      println(numerator * BigInt(denominator).modInverse(p) % p)
    }
  }
}
