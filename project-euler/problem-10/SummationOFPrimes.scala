object Solution {
  def sievePrimeGenerator(n: Int): Array[Int] = {
    val nums = Array.fill(n + 1)(true)
    val primes = for (i <- (2 to n).toIterator if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    primes.toArray
  }

  def sumOFPrimes(primes: Array[Int], n: Int): Long = {
    var i = 0
    var sum = 0l
    while (i < primes.length && primes(i) <= n) {
      sum += primes(i)
      i += 1
    }
    sum
  }
  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution */
    val upperBound = math.pow(10, 6).toInt
    val primes = sievePrimeGenerator(upperBound)
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      println(sumOFPrimes(primes, n))
    }
  }
}
