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
    x match {
      case p if p < 0 => false
      case p if p <= size => primeTable(p.toInt)
      case p if p <= sizeSquare =>
        for (prime <- primes) {
          if (p % prime == 0) return false
        }
        true
      case p =>
        for (prime <- primes) {
          if (p % prime == 0) return false
        }
        var start = (sizeSquare - 5) / 6 * 6 + 5
        while (start * start <= p) {
          if (p % start == 0 || p % (start + 2) == 0) return false
          start += 6
        }
        true
    }
  }
    
  def main(args: Array[String]) {
    val n = readLine.toInt
    val ratio = 1.0 * n / 100
    var i = 3L
    var primeCount = 3L
    var totalCount = 5L
    val (primes, primeTable) = sievePrimeGenerator(1e7.toInt)
    while (1.0 * primeCount / totalCount >= ratio) {
      val iSquare = i * i
      i += 2
      var newPrimeCount = 0
      var j = 1
      while (j <= 3) {
        if (isPrime(iSquare + (i - 1) * j, primes, primeTable)) 
          newPrimeCount += 1
        j += 1
      }
      primeCount += newPrimeCount
      totalCount += 4
    }
    println(i)
  }
}
