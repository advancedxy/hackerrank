object Solution {

  def sievePrimeGenerator(n: Int): Array[Int] = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false
    nums(1)= false
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

  def isSquare(n: Long): (Boolean, Long) = {
    // fail fast
    if (n < 0 || (n & 2) != 0 || (n & 7) == 5 || (n & 11) == 8) return (false, -1)
    if (n == 0) return (true, 0)
    val x = math.sqrt(n).toLong
    (x * x == n, x)
  }

  def numOfConjections(x: Int, primes: Array[Int]): Int = {
    if (x % 2 == 0) 0
    else {
      var i,count = 0
      val size = primes.size
      while(i < size && primes(i) < x) {
        val p = primes(i)
        if (isSquare((x - p) / 2)._1) count += 1
        i += 1
      }
      count
    }
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    val tests = (1 to t).map(x => readLine.toInt)
    // 2 cannot be in the conjection
    val primes = sievePrimeGenerator(tests.max).drop(1)
    tests foreach { x =>
      println(numOfConjections(x, primes))
    }
  }
}
