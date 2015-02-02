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

  def searchConsecutivePrimeSum(n: Long,
                                primes: Array[Int],
                                primeSums: Array[Long]): (Long, Int) = {

    def isPrime(x: Long): Boolean = {
      var i = 0;
      while (i < primes.size && primes(i) * primes(i) <= x) {
        if (x % primes(i) == 0) return false
        i += 1
      }
      true
    }
    
    import java.util.Arrays.binarySearch
    val searchResult = binarySearch(primeSums, n)
    val searchPoint = if (searchResult < 0) -searchResult - 1 else searchResult
    var i = 0
    var j = searchPoint
    var p = 2l
    var count = 0
    // This is totally hack as we cannot guarantee we can get the result in the
    // first 20 items. I think we need some math background support here.
    val searchEntries = 20
    while(i <  searchEntries && i < primeSums.size) {
      j = searchPoint
      while( j > searchPoint - searchEntries && j >= 0) {
        val newSum = primeSums(j) - primeSums(i)
        if (newSum <= n && isPrime(newSum)) {
          if (j - i > count) { count = j - i; p = newSum }
          else if (j - i == count) p = p min newSum
          j = 0
        }
        j -= 1
      }
      i += 1
    }
    (p, count)
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    val tests = (1 to t).map(x => readLine.toLong)
    // Calculate the upper limit. This is also a hack, I cannot make sure the
    // sum of primes is larger than input n. Need some math to support it
    val limit = math.sqrt(tests.max).toInt * 10 
    val (primes, primeTable) = sievePrimeGenerator(limit)
    var i = 0
    var sum = 0l
    val primeSums = Array.fill(primes.size + 1)(0l)
    for (p <- primes) {
      sum += p
      i += 1
      primeSums(i) = sum
    }

    tests.foreach { x =>
      val (sum, cnt) = searchConsecutivePrimeSum(x, primes, primeSums)
      println(sum + " " + cnt)
    }
  }
}
