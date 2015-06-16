object Solution {
  import scala.annotation.tailrec

  @tailrec
  def gcd(m: Int, n: Int): Int = if (m == 0) n else gcd(n % m, m)

  def calculateCountsForCouboid(M: Int): IndexedSeq[Long] = {
    // cuboid, w <= h <= l, shortest = sqrt(l^2 + (w+h)^2)
    // a^2 + b^2 = c^2 => a = d*(m^2 - n^2), b = 2*d*m*n, c = d*(m^2 + n^2)
    // m,n co-prime and exactly one of m, n is even.
    // a <= M, b <= 2*M, c <= sqrt(5)*M => m^2 <=sqrt(sqrt(5)*M)
    val mLimit = math.sqrt(math.sqrt(5) * M + 1).toInt
    val result = Array.fill(M + 1)(0L)
    for {
      m <- 2 to mLimit
      val nStart = if (m % 2 == 0) 1 else 2
      n <- nStart until m by 2 if gcd(m, n) == 1
    } {
      val baseA = m * m - n * n
      val baseB = 2 * m * n
      if (baseA * 2 >= baseB) { 
        val halfbb = m * n
        var j = 1
        while (baseA * j <= M) {
          val c = if (baseA >= baseB) halfbb * j else 1 + j * (baseA - halfbb)
          result(baseA * j) += c
          j += 1
        }
      }
      if (baseB * 2 >= baseA) {
        var j = 1
        while (baseB * j <= M) {
          val c =
            if (baseB >= baseA) baseA * j / 2
            else 1 + j * baseB - math.ceil(j * baseA / 2.0).toInt

          result(baseB * j) += c
          j += 1
        }
      }
    }
    var count = 0L
    for (i <- 0 to M) yield {
      count += result(i)
      count
    }
  }

  def main(args: Array[String]) {
    val t = readInt
    val ms = (1 to t).map(x => readInt)
    val preCachedResult = calculateCountsForCouboid(ms.max)
    for (m <- ms) {
      println(preCachedResult(m))
    }
  }
}
