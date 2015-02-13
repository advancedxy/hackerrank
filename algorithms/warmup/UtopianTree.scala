object Solution {

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val n = readLine.toInt
      val (power, sub) = if ((n & 1) != 0) ((n + 3) / 2, 2) else (n / 2 + 1, 1)
      println(math.pow(2, power).toInt - sub)
    }
  }
}
