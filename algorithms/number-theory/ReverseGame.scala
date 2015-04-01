object Solution {

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(n, k) = readLine.split(" ").map(_.toInt)
      val idx = if (k < (n - 1) / 2) 2 * k + 1 else (n - k - 1) * 2
      println(idx)
    }
  }
}
