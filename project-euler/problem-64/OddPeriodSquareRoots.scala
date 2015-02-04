object Solution {

  def numbOfOddSquareRootPeriod(x: Int): Int = {
    var count = 0
    for (n <- 2 to x) {
      val a0 = math.sqrt(n).toInt
      if (a0 * a0 == n) {}// perfect square, skip
      else {
        var period = 0
        var (d, m, a) = (1, 0, a0)
        while(a != 2 * a0) {
          m = d * a - m
          d = (n - m * m) / d
          a = (a0 + m) / d
          period += 1
        }
        period += 1
        if (period % 2 == 0) count += 1
      }
    }
    count
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    println(numbOfOddSquareRootPeriod(n))
  }
}
