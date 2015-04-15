object Solution {

  // I found this pdf is quite useful
  // https://nrciz.googlecode.com/svn/trunk/projecteuler/pdfs/073_overview.pdf
  // I add this pdf to my repo in case googlecode is dead
  // Will update the code when I get some time.
  def sieveGenerator(n: Int): (IndexedSeq[Int], Array[Boolean]) = {
    val primeTable = Array.fill(n + 1)(true)
    primeTable(0) = false
    primeTable(1) = false
    val primes = for (i <- 2 to n if primeTable(i)) yield {
      var index = i * 2
      while (index >= 0 && index <= n) {
        primeTable(index) = false
        index += i
      }
      i
    }
    (primes, primeTable)
  }

  def fastCountingFractionInRange(a: Int, n: Int): Long = {
    // calculate the number of fractions between 1/(a+1) and 1/a
    val F = Array.fill(n + 1)(0L)
    var count = 0L
    for (i <- 1 to n) {
      count += (i - 1) / a - i / (a + 1)
      F(i) = count
    }

    // use inclusion-exclusion principle to calculate the reduced fraction
    val (primes, _) = sieveGenerator(n / (2 * a + 1))
    val primeCount = primes.size

    // I don't fully understand why we can use this recursive function to get the
    // total number of reduced fractions. It's written in the pdf which I should
    // read again and again to comprehend.
    def inclusionExclusion(limit: Int, index: Int): Long = {
      var count = F(limit)
      var idx = index
      while (idx < primeCount && 1L * (2 * a + 1) * primes(idx) <= limit) {
        val newLimit = limit / primes(idx)
        count -= inclusionExclusion(newLimit, idx + 1)
        idx += 1
      }
      count
    }

    inclusionExclusion(n, 0)
  }

  def main(args: Array[String]) {
    val Array(a, d) = readLine.split(" ").map(_.toInt)
    println(fastCountingFractionInRange(a, d))
  }
}
