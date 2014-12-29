object Solution {
  def diff(n: Int): Long = {
    1l * (3 * n + 2) * (n + 1) * n * (n - 1) / 12
  }
  def main(args: Array[String]) {
    val n = readLine.toInt
    for (i <- 1 to n) {
      val num = readLine.toInt
      println(diff(num))
    }
  }
}
