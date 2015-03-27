object Solution {

  def nums(n: Int, m: Int): Long = {
    val p = 1e9.toInt + 7
    if (n == 1) m
    else if (n == 2) 1L * m * (m - 1) % p
    else {
      val prefix = 1L * m * (m - 1)
      if (prefix == 0) 0L
      else
        (prefix * BigInt(m - 2).modPow(n - 2, p) % p).toLong
    }
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(n, m) = readLine.split(" ").map(_.toInt)
      println(nums(n, m))
    }
  }
}
