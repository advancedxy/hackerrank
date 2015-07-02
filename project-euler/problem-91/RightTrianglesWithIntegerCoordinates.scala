object Solution {
  import scala.annotation.tailrec

  @tailrec
  def gcd(a: Int, b: Int): Int = if (a == 0) b else gcd(b % a, a)

  def numberOfTriangles(n: Int): Long = {
    var p = 3L * n * n
    for {
      x <- 1 to n
      y <- 1 to n
    } {
      val fact = gcd(x, y)
      p += math.min(y * fact / x, (n - x) * fact / y) * 2L
    }
    p
  }

  def main(args: Array[String]) {
    val n = readInt
    println(numberOfTriangles(n))
  }
}
