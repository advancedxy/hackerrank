object Solution {

  def main(args: Array[String]) {
    val p = 1234567
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val _ = readLine
      val routes = readLine.split(" ").map(_.toInt)
      println(routes.foldLeft(1L) { _ * _ % p })
    }
  }
}
