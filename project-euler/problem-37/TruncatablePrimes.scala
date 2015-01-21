object Solution {

  def sievePrimeGenerator(n: Int): (List[Int], Array[Boolean]) = {
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
    (primes.toList, nums)
  }

  def isTruncatablePrime(p: Int, primeTable: Array[Boolean]): Boolean = {
    val digits = p.toString.map(_.asDigit)
    val truncatesFromRight = digits.foldLeft(List(0)) { (x, y) =>
      x.head * 10 + y :: x
    } dropRight(1)
    var base = 1
    var prime = p
    var pp = 0
    val truncatesFromLeft = for (i <- 1 to digits.length) yield {
      pp += (prime % 10) * base
      base *= 10
      prime /= 10
      pp
    }
    (truncatesFromLeft ++ truncatesFromRight).forall(primeTable(_))
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val (primes, primeTable) = sievePrimeGenerator(n - 1)
    val sum = primes.drop(4).filter(x => isTruncatablePrime(x, primeTable)).sum
    println(sum)
  }
}
