object Solution {

   def numberOfCycleDigits(n: Int, d: Int): Int = {
    val remainderMap = collection.mutable.Map.empty[Int, Int]
    var num = n
    for (i <- 1 to d) {
      val remainder = num % d
      if (remainder == 0) return 0
      if (remainderMap.contains(remainder)) {
        return remainderMap.size - remainderMap(remainder) + 1
      }
      remainderMap(remainder) = i
      num = 10 * remainder
    }
    remainderMap.size
  }

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

  def indexArrayGenerator(n: Int): Array[Int] = {
    val (primes, _) = sievePrimeGenerator(n)
    val indexArray = Array.fill(n + 1)(0)
    for (prime <- primes) {
      var num = prime
      while (num <= n) {
        indexArray(num) = numberOfCycleDigits(1, num)
        num *= prime
      }
    }
    var longest = 0
    var index = 1
    for (i <- 2 to n) {
      val count = indexArray(i)
      indexArray(i) = index
      if (count > longest) {
        longest = count
        index = i
      }
    }
    indexArray
  }

  def main(args: Array[String]) {
    val upperBound = 1e4.toInt
    val indexArray =  indexArrayGenerator(upperBound)
    val t = readLine.toInt
    for (i <- 1 to t) {
      var n = readLine.toInt
      println(indexArray(n))
    }
  }
}
