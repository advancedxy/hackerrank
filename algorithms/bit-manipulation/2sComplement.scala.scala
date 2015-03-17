object Solution {

  def bitsFromZeroToN(n: Int): Long = {
    assert(n >= 0)
    def bitsAux(acc: Long, n: Int): Long = {
      if (n <= 1) acc + n
      else {
        val n1 = n + 1l
        val log2 = n1.toBinaryString.length - 1
        val rem = n - (1 << log2) + 1
        bitsAux(acc + log2 * (1l << (log2 - 1)) + rem, (rem - 1) max 0)
      }
    }

    bitsAux(0L, n)
  }

  def bitsFromMinToN(n: Int): Long = {
    assert(n < 0)
    val x = n - Int.MinValue
    x + bitsFromZeroToN(x) + 1
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(l, r) = readLine.split(" ").map(_.toInt)
      val lbits = l.toBinaryString.filter(_ == '1').size
      val result = (l, r) match {
        case (x, y) if y < 0 => bitsFromMinToN(y) - bitsFromMinToN(x) + lbits
        case (x, y) if x < 0 && y >= 0  =>
           bitsFromMinToN(-1) - bitsFromMinToN(x) + lbits + bitsFromZeroToN(y)
        case (x, y) if x >= 0 => bitsFromZeroToN(y) - bitsFromZeroToN(x) + lbits
      }
      println(result)
    }
  }
}
