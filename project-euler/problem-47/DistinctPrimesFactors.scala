object Solution {

  def sievePrimeGenerator(n: Int): (Array[Int], Array[Boolean]) = {
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
    (primes.toArray, nums)
  }

  def genPrimeFactorNum(n: Int): Array[Int] = {
    val result = Array.fill(n + 1)(1)
    val (primes, primeTable) = sievePrimeGenerator(n)
    for (i <- 2 to n if !primeTable(i)) {
      val index = primes.indexWhere(i % _ == 0)
      val p = if (index < 0) 1 else primes(index) // p will never be 1
      val pp = i / p
      result(i) = if (pp % p == 0) result(i / p) else result(i / p) + 1
    }
    result
  }
  
  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val numOfPrimeFactors = genPrimeFactorNum(n + k)
    val nums = (6 until n + k).filter { x =>
      numOfPrimeFactors(x) == k
    }
    for (ns <- nums.sliding(k)) {
      if (ns.zip(ns.slice(1, ns.size)).forall(x => x._2 - x._1 == 1)) println(ns(0))
    }
  }
}
