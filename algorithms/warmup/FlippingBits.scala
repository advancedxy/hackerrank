object Solution {

  def main(args: Array[String]) {
    val bits = 0xFFFFFFFFL
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toLong
      println(bits ^ n)
    }
  }
}
