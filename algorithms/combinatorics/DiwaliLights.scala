object Solution {

  def main(args: Array[String]) {
    val p = 1e5.toInt
    for (_ <- 1 to readLine.toInt) {
      val n = readLine.toInt
      println(BigInt(2).modPow(n, p) - 1)
    }
  }
}
