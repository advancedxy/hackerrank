object Solution {
  import java.util.Arrays.binarySearch

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
    a * math.pow(10, math.log10(b).toInt + 1).toInt + b

  def genKPrimeSet(n: Int, k: Int) = {
    val primeLimit = math.pow(10, math.log10(n).toInt + 1).toInt
    val (primes, primeTable) = sievePrimeGenerator(primeLimit)
    val primeLessThanN = primes.filter(_ < n)

    def isPrime(x: Int): Boolean = {
      if (x < primeLimit) primeTable(x)
      else {
        var j = 0
        while (j < primes.size && primes(j) * primes(j) <= x) {
          if (x % primes(j) == 0) return false
          j += 1
        }
        true
      }
    }

    def isPrimePair(x: Int, y: Int): Boolean = {
      if (x == y) false
      else if (x < y) isPrimePair(y, x)
      else {
        isPrime(concatInts(x, y)) && isPrime(concatInts(y, x))
      }
    }

    lazy val getConcatableSet: Memo[Int, Set[Int]] = Memo {
     case x =>
       val searchPoint = binarySearch(primeLessThanN, x)
       val index = if (searchPoint < 0) -searchPoint - 1 else searchPoint
       primeLessThanN.slice(index, primeLessThanN.size)
             .filter(p => p > x && isPrimePair(p, x))
             .toSet
    }

    def combine(n: Int, ls: Set[Int]): Set[Set[Int]] =
      if (n == 1) ls.map(Set(_))
      else for {
        p <- ls
        filtered = getConcatableSet(p)
        lss <- combine(n - 1, ls & filtered)
      } yield lss + p

    combine(k, primeLessThanN.toSet)
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val kPrimes = genKPrimeSet(n, k)
    for (sum <- kPrimes.toList.map(_.sum).sorted) {
      println(sum)
    }
  }
}
