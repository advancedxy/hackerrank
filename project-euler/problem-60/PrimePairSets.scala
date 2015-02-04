object Solution {

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache getOrElseUpdate (x, f(x))
  }

  def sievePrimeGenerator(n: Int): (Array[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false
    nums(1) = false
    val primes = for (i <- (2 to n).toArray if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    (primes, nums)
  }

  def concatInts(a: Int, b: Int) =
    if (b == 0) a * 10
    else {
      var temp = b
      var result = a
      while (temp > 0) {
        result *= 10
        temp /= 10
      }
      result + b
    }

  def genKPrimeSet(n: Int, k: Int) = {
    val (primes, primeTable) = sievePrimeGenerator(n)

    def isPrime(x: Int): Boolean = {
      if (x <= n) primeTable(x)
      else {
        var j = 0
        while (j < primes.size && primes(j) * primes(j) <= x) {
          if (x % primes(j) == 0) return false
          j += 1
        }
        true
      }
    }
    
    lazy val isPrimePair: Memo[(Int, Int), Boolean] = Memo {
      case (x, y) if x == y => false
      case (x, y) if x > y => isPrimePair(y, x)
      case (x, y) =>
        isPrime(concatInts(x, y)) && isPrime(concatInts(y, x))
    }

    def isCoPrimePair(ls: Seq[Int]): Boolean = {
      ls.combinations(2).forall(x => isPrimePair(x(0), x(1)))
    }
    
    import scala.collection.mutable.{HashMap, ListBuffer}
    val pairSet = new HashMap[Int, ListBuffer[Int]] {
      override def default(k: Int) = ListBuffer.empty[Int]
    }
    
    for {
      i <- 0 until primes.size
      j <- i + 1 until primes.size
    } {
      if (isPrimePair(primes(i), primes(j)))
        pairSet(primes(i)) = pairSet(primes(i)) += primes(j)
    }

    // Ok, we get here, we can filter through the pairs
    pairSet.filter(_._2.size > k - 1).flatMap( {
      case (key, v) => v.combinations(k - 1)
                        .filter(isCoPrimePair(_)).map(key +: _)
    }
    )
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val kPrimes = genKPrimeSet(n - 1, k)
    for (sum <- kPrimes.map(_.sum).toList.sorted) {
      println(sum)
    }
  }
}
