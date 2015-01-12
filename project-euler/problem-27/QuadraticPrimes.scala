object Solution {
  val upperBound = 2000
  val abArray = Array.fill(upperBound + 1)((0, 0, 0))

  def sievePrimeGenerator(n: Int): Array[Boolean] = {
    val nums = Array.fill(n + 1)(true)
    val primes = for (i <- (2 to n) if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    nums
  }

  def caculteIndexes {
    val primes = sievePrimeGenerator(upperBound * 80 * 2 + upperBound)
    def consecutivePrimesCount(a: Int, b: Int): Int = {
      Stream.from(0).map(x => x * x + a * x + b)
        .takeWhile(x => x >= 2 && primes(x)).length
    }
    var max,a,b = 0
    for (index <- 0 to upperBound) {
      for (i <- -index to index) {
        val c1 = consecutivePrimesCount(index, i)
        if (c1 > max) {
          max = c1
          a = index
          b = i
        }
        val c2 = consecutivePrimesCount(i, index)
        if (c2 > max) {
          max = c2
          a = i
          b = index
        }
      }
      abArray(index) = (a, b, max)
    }
  }
  def main(args: Array[String]) {
    caculteIndexes
    val n = readLine.toInt
    val (a, b, _) = abArray(n)
    println(a + " " + b)
  }
}
