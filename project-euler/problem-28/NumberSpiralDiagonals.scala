object Solution {
  val moduloP = 1e9.toInt + 7

  def diagonalSum(x: Long): Long = {
    val m = x / 2
    val multiples = m % 3 match {
      case 0 => List(m / 3, m + 1, x)
      case 1 => List(m, m + 1, x / 3)
      case 2 => List(m, (m + 1) / 3, x)
    }
    (1 + (multiples.foldLeft(8l) { case (x, y) =>
      (x * (y % moduloP)) % moduloP
    }) + 2 * (m % moduloP) * (m % moduloP) + 6 * (m % moduloP)) % moduloP
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toLong
      println(diagonalSum(n))
    }
  }
}
