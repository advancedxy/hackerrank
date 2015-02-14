object Solution {

  def main(args: Array[String]) {
    val Array(n, m) = readLine.split(" ").map(_.toInt)
    var sum = 0L
    for (_ <- 1 to m) {
      val Array(a, b, k) = readLine.split(" ").map(_.toInt)
      sum += 1L * (b - a + 1) * k // (b - a + 1) * k may overflow.
    }
    println(sum / n)
  }
}
