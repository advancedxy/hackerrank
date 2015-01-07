object Solution {

  def factorial(n: Int): BigInt = (2 to n).foldLeft(BigInt(1)) { _ * _}

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      val sum = factorial(n).toString.map(_.asDigit).sum
      println(sum)
    }
  }
}
