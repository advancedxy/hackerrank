object Solution {

  def sievePrimeGenerator(n: Int): (List[Int], Array[Boolean]) = {
    val nums = Array.fill(n + 1)(true)
    nums(0) = false
    nums(1) = false
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
    result(1) = Map(1 -> 1)
    result
  }
  
  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val primeFactors = primeFactorMap(n + k)
    val nums = (2 until n + k).filter(primeFactors(_).size == k)
    var i = 1
    var cnt = 1
    while(i < nums.size) {
      if (nums(i) - nums(i - 1) == 1) {
        cnt += 1
      } else {
        for (j <- 0 to cnt - k) {
          println(nums(j - cnt + i))
        }
        cnt = 1
      }
      i += 1
    }
    for (j <- 0 to cnt -k) {
      println(nums(i - cnt + j))
    }
  }

}
