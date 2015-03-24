object Solution {

  def sievePrimeGenerator(n: Int): (Array[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false
    nums(1) = false
    val primes = for (i <- (2 to n).toIterator if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    (primes.toArray, nums)
  }

  def isPrime(x: Long,
              primes: Array[Int],
              primeTable: Array[Boolean]): Boolean = {
    val size = primeTable.size - 1
    val sizeSquare = 1L * size * size
    require(sizeSquare >= x)
    val pSize = primes.size
    if (x < 0) false
    else if (x <= size) primeTable(x.toInt)
    else {
      var j = 0
      val sqrtX = math.sqrt(x).toLong
      while (j < pSize && primes(j) <= sqrtX) {
        if (x % primes(j) == 0) return false
        j += 1
      }
      true
    }
  }

  def getIUnderRatio(ratio: Double): Long = {
    var sl = 2L
    var primeCount = 3L
    var p = 9L
    // 24e4 is a magical number, as I pre-compute the answer.
    val (primes, primeTable) = sievePrimeGenerator(24e4.toInt)
    while (primeCount  >= ratio * (2 * sl + 1)) {
      sl += 2
      var i = 1
      while (i <= 3) {
        p += sl
        // println(p)
        if (isPrime(p, primes, primeTable)) primeCount += 1
        i += 1
      }
      p += sl
    }
    sl + 1
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val ratio = 1.0 * n / 100
    val i = getIUnderRatio(ratio)
    println(i)
  }
}
