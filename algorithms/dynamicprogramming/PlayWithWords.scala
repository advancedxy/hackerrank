object Solution {

  def maxProduct(s: String): Int = {
    val n = s.size
    // longest palindromic subsequences count.
    val lpc = (0 until n).map(x => Array.fill(n)(0)).toArray

    for (i <- 0 until n) lpc(i)(i) = 1 // 'x' is 1 size palindromic

    for {
      cl <- 2 to n
      i <- 0 until n -cl + 1
    } {
      val j = i + cl - 1
      if (s(i) == s(j)) lpc(i)(j) = lpc(i + 1)(j - 1) + 2
      else lpc(i)(j) = lpc(i)(j - 1) max lpc(i + 1)(j)
    }

    //println(lpc.map(_.mkString(" ")).mkString("\n"))
    // we can get the lpc now
    (0 until n - 1).map(x => lpc(0)(x) * lpc(x + 1)(n - 1)).max
  }

  def main(args: Array[String]) {

    val s = readLine
    println(maxProduct(s))
  }
}
