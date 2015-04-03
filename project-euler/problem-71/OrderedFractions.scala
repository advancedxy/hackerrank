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

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(a, b, n) = readLine.split(" ").map(_.toLong)
      val (r, s) = (nearestFraction(a, b, n))
      println(r + " " + s)
    }
  }
}
