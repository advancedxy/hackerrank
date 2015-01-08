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

  def main(args: Array[String]) {
    val upperBound = 1e5.toInt
    //val divisorMap = divisorGenerator(upperBound)
    //val sumOfProperDivisorMap = divisorMap.zipWithIndex.map { case (x, y) =>
      //x.sum - y
    //}

    val primeFactorMapArray  = primeFactorMap(upperBound)
    val sumOfProperDivisorMap = primeFactorMapArray.zipWithIndex map {
      case (x, y) =>
        (x.foldLeft(1.0) { (product, pf) => product * (math.pow(pf._1, pf._2 + 1) - 1) / (pf._1 - 1) }).toInt - y
    }
    val amicableNumbers = for {
      j <- 2 until upperBound
      if sumOfProperDivisorMap(j) != j && sumOfProperDivisorMap(j) < upperBound &&
        sumOfProperDivisorMap(sumOfProperDivisorMap(j)) == j
    } yield j

    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      println(amicableNumbers.takeWhile(_ < n).sum)
    }
  }
}
