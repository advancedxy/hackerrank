object Solution {

  def nonMersennePrimeRSD(a: Int, b: Int, c: Int, d: Int, p: Long): Long = {
    // Fucking BigInt to long overflow! Must mod first.
    (a * BigInt(b).modPow(c, p) + d).mod(p).toLong
  }

  def main(args: Array[String]) {
    val t = readInt
    val p = 1e12.toLong
    var sum = 0L
    for (i <- 1 to t) {
      val Array(a, b, c, d) = readLine.split(" ").map(_.toInt)
      sum = (sum + nonMersennePrimeRSD(a, b, c, d, p)) % p
    }
    println("%012d".format(sum))
  }
}
