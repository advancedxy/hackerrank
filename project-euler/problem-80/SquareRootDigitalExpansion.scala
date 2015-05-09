object Solution {
  import java.math.BigDecimal.ROUND_HALF_UP
  import java.math.BigDecimal

  def squareRoot(i: Int, nDigits: Int): BigInt = {
    val limit = BigInt(10).pow(nDigits + 1)
    var a = 5 * BigInt(i)
    var b = BigInt(5)
    while (b < limit) {
      if (a >= b) {
        a = a - b
        b += 10
      }
      else {
        a *= 100
        b = 10 * b - 45
      }
    }
    b / 100
  }

  val ZERO = new BigDecimal("0")
  val TWO = new BigDecimal("2")
  def sqrtBigDecimal(A: BigDecimal, SCALE: Int) = {
    var x0 = ZERO
    var x1 = new BigDecimal(math.sqrt(A.intValue));
    while (!x0.equals(x1)) {
        x0 = x1
        x1 = A.divide(x0, SCALE, ROUND_HALF_UP).add(x0)
        x1 = x1.divide(TWO, SCALE, ROUND_HALF_UP);
    }
    x1
  }

  def isPerfectRoot(x: Int): Boolean = {
    val sqrt = math.sqrt(x).toInt
    sqrt * sqrt == x
  }

  def main(args: Array[String]) {
    val n = readInt
    val p = readInt
    val sum = (1 to n).filter(!isPerfectRoot(_))
      .map(x =>
        sqrtBigDecimal(new BigDecimal(x), p + 1).toString.take(p + 1)
          .foldLeft(0) { (x, y) => if (y == '.') x else x + y.asDigit }).sum
    println(sum)
  }
}
