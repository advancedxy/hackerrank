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
  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution */
    val upperBound = math.pow(10, 4).toInt
    val number = (2 * upperBound * math.log(upperBound)).toInt
    val primes = sievePrimeGenerator(number)
    val t = readLine.toInt
    for (i <- 1 to t) {
      val nth = readLine.toInt
      println(primes(nth - 1))
    }
  }
}
