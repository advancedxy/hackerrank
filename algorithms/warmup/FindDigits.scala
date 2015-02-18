object Solution {

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val n = readLine
      val num = n.toLong
      println(n.map(_.asDigit).filter(x => x != 0 && num % x == 0).size)
    }
  }
}
