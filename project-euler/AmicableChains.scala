object Solution {
  import scala.collection.mutable.LinkedHashSet

  def sievePrimeGenerator(n: Int): (List[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false; nums(1) = false
    val primes = for (i <- (2 to n).toList if nums(i)) yield {
      var j = 2
      while (i * j <= n) {
        nums(i * j) = false
        j += 1
      }
      i
    }
    (primes, nums)
  }

  def divisorGenerator(n: Int): Array[Vector[Int]] = {
    val result = Array.fill(n + 1)(Vector.empty[Int])
    val (primes, primeTable) = sievePrimeGenerator(n)
    for (i <- 1 to n) {
      if (primeTable(i)) result(i) = Vector(1, i)
      else {
        val p = primes.dropWhile(i % _ != 0).head
        result(i) = (result(i / p) ++ result(i / p).map(p * _)).distinct
      }
    }
    result
  }

  def primeFactorMap(n: Int): Array[Map[Int, Int]] = {
    val result = Array.fill(n + 1)(Map.empty[Int, Int])
    val (primes, primeTable) = sievePrimeGenerator(n)
    for (i <- 2 to n) {
      if (primeTable(i)) result(i) =  Map(i -> 1)
      else {
        val p = primes.dropWhile(i % _ != 0).head
        val pExp = result(i / p).getOrElse(p, 0)
        result(i) = result(i / p).updated(p, pExp + 1)
      }
    }
    result
  }

  def sumOfTotalDivisors(primeMap: Map[Int, Int]): Int = {
    primeMap.foldLeft(1) { (result, pa) =>
      val (p, a) = pa
      if (a == 1)  (p + 1) * result
      else (math.pow(p, a + 1) - 1) / (p - 1) * result toInt
    }
  }

  def main_(upperBound: Int): Seq[Int] = {
    val primeFactorMapArray  = primeFactorMap(upperBound)
    val sumOfProperDivisorMap = Array.fill(upperBound + 1)(0)
    for (i <- 2 to upperBound) {
      sumOfProperDivisorMap(i) = sumOfTotalDivisors(primeFactorMapArray(i)) - i
    }

    val amicableChainLength = Array.fill(upperBound + 1)(-1)
    amicableChainLength(0) = 0; amicableChainLength(1) = 0


    def chainHelper(lastIdx:Int, idx: LinkedHashSet[Int]) {
      val next = sumOfProperDivisorMap(lastIdx)
      if (next > upperBound || amicableChainLength(next) >= 0) {
        idx.foreach(x => amicableChainLength(x) = 0)
      } else if (idx(next)) {
        val lst = idx.toList
        val (left, right) = lst.splitAt(lst.indexOf(next))
        val chainLength = idx.size - left.size
        left.foreach(x => amicableChainLength(x) = 0)
        right.foreach(x => amicableChainLength(x) = chainLength)
      }
      else chainHelper(next, idx + next)
    }
    for (i <- 2 to upperBound if amicableChainLength(i) < 0) {
      chainHelper(i, LinkedHashSet(i))
    }

    amicableChainLength
  }

  def main(args: Array[String]) {
    val upperBound = readInt
    val amicableChainLength = main_(upperBound)
    val longestLength = amicableChainLength.max
    println(amicableChainLength.indexOf(longestLength))
  }
}
