object Solution {

  def sieveGenerator(n: Int): (IndexedSeq[Int], Array[Boolean]) = {
    val primeTable = Array.fill(n + 1)(true)
    primeTable(0) = false
    primeTable(1) = false
    val primes = for (i <- 2 to n if primeTable(i)) yield {
      var q = i * 2
      while (q <= n) {
        primeTable(q) = false
        q += i
      }
      i
    }
    (primes, primeTable)
  }

  def countTable(choices: IndexedSeq[Int], n: Int): Array[Long] = {
    val ways = Array.fill(n + 1)(0L)
    ways(0) = 1L
    for {i <- 0 until choices.length
         j <- choices(i) to n} {
      ways(j) = ways(j) + ways(j - choices(i))
    }
    ways
  }

  def main(args: Array[String]) {
    val t = readInt
    val tcs = (1 to t).map(x => readInt)
    val (primes, primeTable) = sieveGenerator(tcs.max)
    val result = countTable(primes, tcs.max)
    for (t <- tcs) {
      println(result(t))
    }
  }
}
