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
      val p = primes.find(i % _ == 0).getOrElse(1)
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
    var i = 1
    var cnt = 1
    while(i < nums.size) {
      if (nums(i) - nums(i - 1) == 1) {
        cnt += 1
        if (i == nums.size - 1) { // last element should print nums
          for (j <- 0 to cnt - k) {
            println(nums(j - cnt + i + 1))
          }
        }
      } else {
        for (j <- 0 to cnt - k) {
          println(nums(j - cnt + i))
        }
        cnt = 1
      }
      i += 1
    }
  }
}
