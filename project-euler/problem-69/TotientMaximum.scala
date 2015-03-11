object Solution {

  // see http://en.wikipedia.org/wiki/Euler's_totient_function
  def sievePrimeGenerator(n: Int): (Seq[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false
    nums(1) = false
    val primes = for (i <- (2 to n) if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    (primes, nums)
  }
  
  val (primes, _) = sievePrimeGenerator(1e3.toInt + 1)

  def maxTotient(n: Long): Long = {
    var i = 0; var r = 1L
    // to deal with Long overflow
    while (i < primes.size && r <= n / primes(i)) {
      r = primes(i) * r
      i += 1
    }

    if (r >= n) r / primes(i - 1) else r
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val n = readLong
      println(maxTotient(n))
    }
  }
}
