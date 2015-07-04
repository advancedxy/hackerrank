object Solution {

  def almostStream(a: Long, b: Long, c: Long): Stream[Long] ={
    Stream.cons(2 * (a + c),
                almostStream(-2 * a + b + 2 * c,
                             -a + 2 * b + 2 * c,
                             -2 * a + 2 * b + 3 * c))
  }

  val stream = almostStream(3, 4, 5)
  def solve(limit: Long) = {
    stream.takeWhile(_ <= limit).sum
  }

  def main(args: Array[String]) {
    val t = readInt
    for (i <- 1 to t) {
      val n = readLong
      println(solve(n))
    }
  }
}
