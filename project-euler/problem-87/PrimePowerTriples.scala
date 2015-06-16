object Solution {

  def sieveGenerator(n: Int): IndexedSeq[Int] = {
    val primeTable = Array.fill(n+1)(true)
    primeTable(0) = false
    primeTable(1) = false
    val primes = for (i <- 2 to n if primeTable(i)) yield {
      var j = 2
      while (i * j <= n) {
        primeTable(i * j) = false
        j += 1
      }
      i
    }
    primes
  }

  def calculateTriplets(num: Int): Array[Int] = {
    val hitTable = Array.fill(num + 1)(false)
    val primes = sieveGenerator(math.sqrt(num).toInt + 1)
    val powers =
      (2 to 4).map { x =>
        primes.map(p => math.pow(p, x).toLong).filter(x => x <= num)
      }
    for {
      i <- 0 until powers(0).length
      j <- 0 until powers(1).length
      k <- 0 until powers(2).length
      val number = powers(0)(i) + powers(1)(j) + powers(2)(k)
      if number <= num
    } {
      hitTable(number.toInt) = true
    }
    val result = Array.fill(num + 1)(0)
    var count = 0
    for (i <- 0 to num) {
      if (hitTable(i)) count += 1
      result(i) = count
    }
    result
  }

  def main(args: Array[String]) {
    val t = readInt
    val nums = (1 to t).map(x => readInt)
    val preCachedResult = calculateTriplets(nums.max)
    for (n <- nums) {
      println(preCachedResult(n))
    }
  }
}
