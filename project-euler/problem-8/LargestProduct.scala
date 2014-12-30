object Solution {
  def largest(digits: Array[Int], k: Int): Int = {
    var max,iter = digits.take(k).reduce(_ * _)
    for (i <- k until digits.length) {
      val preDigit = digits(i - k)
      iter= if (preDigit != 0) iter / preDigit * digits(i) else digits.slice(i - k + 1, i + 1).reduce(_ * _)
      if (iter > max) max = iter
    }
    max
  }
  def main(args: Array[String]) {
    val n = readLine.toInt
    for (i <- 1 to n) {
      val Array(_, k) = readLine.split(" ").map(_.toInt)
      val digits = readLine.map(_.asDigit).toArray
      println(largest(digits, k))
    }
  }
}
