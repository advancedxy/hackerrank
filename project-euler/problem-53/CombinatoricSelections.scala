object Solution {
  
  def numOfRLargerThanK(n: Int, k: Long): Int = {
    var c = 1l
    var r = 0

    // deal with Long overflow
    while (r <= n / 2 && c / (r + 1) <= k / (n - r)) {
      // c * (n - r) may overflow
      if ((n - r) % (r + 1) == 0) c = c * ((n - r) / (r + 1))
      else c = c / (r + 1) * (n - r)
      r += 1
    }
    // c will close to be
    if (c <= k) r += 1
    if (r > n / 2) 0 else n - 2 * r + 1
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toLong)
    println((1 to n.toInt).map(numOfRLargerThanK(_, k)).sum)
  }
}
