object Solution {

  def isSquare(n: Long): (Boolean, Long) = {
    // fail fast
    if (n < 0 || (n & 2) != 0 || (n & 7) == 5 || (n & 11) == 8) return (false, -1)
    if (n == 0) return (true, 0)
    val x = math.sqrt(n).toLong
    (x * x == n, x)
  }

  def main(args: Array[String]) {
    val Array(n, a, b) = readLine.split(" ").map(_.toLong)
    (a, b) match {
      case (3, 5) =>
        var m, p = 1L
        while (p < n) {
          if (isSquare(8 * p + 1)._1) println(p)
          m += 1
          p = m * (3 * m - 1) / 2
        }
      case (5, 6) =>
        var m, p = 1L
        while (p < n) {
          val (isSq, root) = isSquare(8 * p + 1)
          if (isSq && ((root + 1) & 3) == 0) println(p)
          m += 1
          p = m * (3 * m - 1) / 2
        }
      case _ =>
    }
  }
}
