object Solution {
  val upperBound = 2000
  val indexArray = Array.fill(upperBound + 1)(0)

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

  def caculteIndexes {
    val primes = sievePrimeGenerator(upperBound * 80 * 2 + upperBound)
    def consecutivePrimesCount(a: Int, b: Int): Int = {
      Stream.from(0).map(x => x * x + a * x + b).
        takeWhile(x => x >= 2 && primes(x)).count
    }
    var max = 0
    for (index <- 0 to upperBound) {
      for (i <- 0 until index)
    }
  }
  def main(args: Array[String]) {

  }
}
