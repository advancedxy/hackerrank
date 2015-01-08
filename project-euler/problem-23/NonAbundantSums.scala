object Solution {

  def sievePrimeGenerator(n: Int): (List[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
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

  def main(args: Array[String]) {
    val upperBound = 1e5.toInt
    val primeFactorMapArray  = primeFactorMap(upperBound)
    val sumOfProperDivisorMap = primeFactorMapArray.zipWithIndex map {
      case (x, y) => sumOfTotalDivisors(x) - y
    }
    
    val abundantNumbers = (2 to upperBound).filter(x => sumOfProperDivisorMap(x) > x)
    val abundantNumbersSet = abundantNumbers.toSet
    def lookUpSum(sum: Int): Boolean = {
      var i = 0
      while(i < abundantNumbers.length && abundantNumbers(i) < sum) {
        if (abundantNumbersSet(sum - abundantNumbers(i))) return true

        i += 1
      }
      false
    }

    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      if (lookUpSum(n)) println("YES")
      else println("NO")
    }
  }
}
