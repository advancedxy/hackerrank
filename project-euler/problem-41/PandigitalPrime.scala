object Solution {
  import scala.collection.Searching._

  def isPrime(n: Long): Boolean = {
    val smallPrimes = List(2, 3, 5, 7)
    if (n < 10) smallPrimes.contains(n)
    else {
      val canBeDivided = smallPrimes.exists(n % _ == 0)
      if (canBeDivided) return false
      var s = 11
      while (s * s < n) {
        if (n % s == 0) return false
        s += 1
      }
      true
    }
  }

  val pandigitalPrimeTable = Array.fill(10)(Vector.empty[Long])
  for (i <- 4 to 9 if (i * (i + 1) / 2) % 3 != 0) {
    val primeList = (1 to i).permutations.filter({ x =>
      val lastDigit = x(i - 1)
      if (lastDigit % 2 == 0 || lastDigit == 5) false
      else {
        val p = x.foldLeft(0l) { (x,y) => x * 10 + y }
        isPrime(p)
      }
    }).map({ x => x.foldLeft(0l) { (x,y) => x * 10 + y } }).toVector
    pandigitalPrimeTable(i) = primeList
  }

  def getLargestPrime(n: Long): Long = {
      val numOfDigits = math.min(n.toString.size, 9)
      val primes = pandigitalPrimeTable(numOfDigits)
      var prime = -1l
      if (primes.size > 0 && primes(0) <= n)
        prime = primes.search(n) match {
          case Found(idx) => n
          case InsertionPoint(idx) => primes(idx - 1)
        }
      else {
        (numOfDigits - 1 to 4 by -1) exists { x =>
          val size = pandigitalPrimeTable(x).size
          if (size > 0) { prime = pandigitalPrimeTable(x)(size - 1); true }
          else false
        }
      }
      prime
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toLong
      println(getLargestPrime(n))
    }
  }
}
