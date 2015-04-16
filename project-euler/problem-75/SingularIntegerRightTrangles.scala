object Solution {
  import scala.annotation.tailrec

  @tailrec
  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  def generateNumbers(x: Int): IndexedSeq[Int] = {
    val triangles = Array.fill(x + 1)(0)
    val mLimit = math.sqrt(x / 2).toInt
    for {
      m <- 2 to mLimit
      n <- (m & 1) + 1 until m by 2
      if gcd(m, n) == 1
    } {
      val a = m * m + n * n
      val b = m * m - n * n
      val c = 2 * m * n
      println(m, n)
      println(a, b, c)
      val p = a + b + c
      for (j <- p to x by p) triangles(j) += 1
    }

    var count = 0
    for (i <- 0 to x) yield {
      if (triangles(i) == 1) count += 1
      count
    }
  }

  def main(args: Array[String]) {
    val t = readInt
    val tcs = (1 to t).map(x => readInt)
    val result = generateNumbers(tcs.max)
    for (t <- tcs) {
      println(result(t))
    }
  }
}
