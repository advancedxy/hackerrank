object Solution {
  import scala.annotation.tailrec

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def fractionLessThan(ln: Long, ld: Long, rn: Long, rd: Long): Boolean = {
    BigInt(ln) * rd < BigInt(rn) * ld
  }
  // See http://en.wikipedia.org/wiki/Farey_sequence
  // We can binary search the Farey Sequence
  def nearestFraction(a: Long, b: Long, n: Long): (Long, Long) = {
    var (ln, rn, ld, rd, md, mn) = (0L, 1L, 1L, 1L, 0L, 0L)
    while (ld + rd <= n) {
      mn = ln + rn
      md = ld + rd
      if (fractionLessThan(a, b, mn, md)) {
        rn = mn
        rd = md
      } else if (a == mn && b == md) {
        // Once we found the targeted fraction, we can just jump to the nearest
        // left fraction. Otherwise we have to iterate about another n / b
        // rounds which is O(n) if b is small.
        val nums = (n - ld - b) / b
        ln = ln + a * nums
        ld = ld + b * nums
        rn = mn
        rd = md
      } else {
        ln = mn
        ld = md
      }
    }
    (ln, ld)
  }

  def countsInRange(a: Int, d: Int): Long = {
    val x = nearestFraction(1, a + 1, d)
    var (n1, n2) = (x._1.toInt, x._2.toInt)
    var result = 0L
    var (n3, n4) = (1, a + 1)
    while(fractionLessThan(n3, n4, 1, a)) {
      val k = (d + n2) / n4
      val p = k * n3 - n1
      val q = k * n4 - n2
      n1 = n3; n2 = n4; n3 = p; n4 = q
      result += 1
    }
    result - 1
  }

  def main(args: Array[String]) {
    val Array(a, d) = readLine.split(" ").map(_.toInt)
    println(countsInRange(a, d))
  }
}
