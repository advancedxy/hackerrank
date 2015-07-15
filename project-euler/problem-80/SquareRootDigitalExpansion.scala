object Solution {
  import java.math.BigDecimal.ROUND_HALF_UP
  import java.math.BigDecimal

  def sievePrimeGenerator(n: Int): (Array[Int], Array[Boolean]) = {
    val primeTable = Array.fill(n + 1)(true)
    primeTable(0) = false
    primeTable(1) = false
    val primes = for (i <- 2 to n if primeTable(i)) yield {
      var j = 2
      while (i * j <= n) {
        primeTable(i * j) = false
        j += 1
      }
      i
    }
    (primes.toArray, primeTable)
  }

  // the maximum n can only be 1000
  val nLimit = 1000
  val (primes, primeTable) = sievePrimeGenerator(nLimit)
  val bigDecimalCache = (0 to nLimit).map(x => BigDecimal.valueOf(x)).toArray

  val ZERO = new BigDecimal("0")
  val TWO = new BigDecimal("2")
  def sqrtBigDecimal(A: BigDecimal, SCALE: Int) = {
    val aInt = A.intValue
    var x0 = ZERO
    var x1 = new BigDecimal(math.sqrt(A.intValue));
    if (! primeTable(aInt)) {
      val base = primes.find(aInt % _ == 0).get
      val rest = aInt / base
      if (isPerfectRoot(rest)){
        val sqrt = math.sqrt(rest).toInt
        x1 = bigDecimalCache(base).multiply(BigDecimal.valueOf(sqrt)).setScale(SCALE, ROUND_HALF_UP)
      } else{
        x1 = bigDecimalCache(base).multiply(bigDecimalCache(rest)).setScale(SCALE, ROUND_HALF_UP)
      }
    }
    while (!x0.equals(x1)) {
        x0 = x1
        x1 = A.divide(x0, SCALE, ROUND_HALF_UP).add(x0)
        x1 = x1.divide(TWO, SCALE, ROUND_HALF_UP)
    }
    bigDecimalCache(aInt) = x1
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
      .map(x => sqrtBigDecimal(new BigDecimal(x), p + 10).toString.take(p + 1)
             .foldLeft(0) { (x, y) => if (y == '.') x else x + y.asDigit })
      .sum

    println(sum)
  }
}
