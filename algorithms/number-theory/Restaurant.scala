object Solution {

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(l, b) = readLine.split(" ").map(_.toInt)
      val g = gcd(l, b)
      println(l * b / g / g)
    }
  }
}
