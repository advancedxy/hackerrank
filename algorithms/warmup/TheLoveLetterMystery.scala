object Solution {

  def numberOfReduceOps(s: String): Int = {
    val size = s.size
    (0 until size / 2).map(i => math.abs(s(i) - s(size - i - 1))).sum
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val s = readLine
      println(numberOfReduceOps(s))
    }
  }
}
