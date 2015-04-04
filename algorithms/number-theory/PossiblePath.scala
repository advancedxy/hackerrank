object Solution {
  import scala.annotation.tailrec

  @tailrec
  def gcd(x: Long, y: Long): Long = if (y == 0) x else gcd(y, x % y)

  def canBeReached(a: Long, b: Long, x: Long, y: Long) = gcd(a, b) == gcd(x, y)

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(a, b, x, y) = readLine.split(" ").map(_.toLong)
      if (canBeReached(a, b, x, y)) println("YES") else println("NO")
    }
  }
}
