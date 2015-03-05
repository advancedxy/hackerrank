object Solution {

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val n = readLine.toInt
      println(1L * n * (n - 1) / 2)
    }
  }
}
