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
    val pSize = primes.size
    if (x < 0) false
    else if (x < size) primeTable(x.toInt)
    else if (x < sizeSquare) {
      var j = 0
      while (j < pSize) {
        if (x % primes(j) == 0) return false
        j += 1
      }
      true
    }
    else {
      var j = 0
      while (j < pSize) {
        if (x % primes(j) == 0) return false
        j += 1
      }
      var start = (sizeSquare - 5) / 6 * 6 + 5
      while (start <= x / start) {
        if (x % start == 0 || x % (start + 2) == 0) return false
        start += 6
      }
      true
    }
  }
    
  def isPrime(n: Long): Boolean = {
    if (n <= 1) return false
    if (n == 2 || n == 3) return true
    if (n % 2 == 0 || n % 3 == 0) return false

    var counter = 5
    while (counter <= n / counter) {
      if (n % counter == 0) return false
      if (n % (counter + 2) == 0) return false
      counter += 6
    }

    true
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val ratio = 1.0 * n / 100
    var sl = 2L
    var primeCount = 3L
    var p = 9L
    val (primes, primeTable) = sievePrimeGenerator(1e6.toInt)
    while (1.0 * primeCount / (2 * sl + 1) >= ratio) {
      sl += 2
      var i = 1
      while (i <= 3){
        p += sl
        if (isPrime(p, primes, primeTable)) primeCount += 1
        i += 1
      }
      p += sl
    }
    println(sl + 1)
  }
}
