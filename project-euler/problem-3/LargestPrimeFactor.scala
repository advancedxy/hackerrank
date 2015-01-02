object Solution {

  def isPrime(n: Long, start: Long): Boolean = {
    var i = start
    while(i * i <= n) {
      if (n % i == 0) return false
      i += 1
    }
    return true
  }

  def largestPrimeFactor(n: Long): Long = {
    var counter = 3L
    var num = n
    while (num % 2 == 0) {
      num /= 2
    }
    if (num == 1) return 2L
    var testFlag = true
    while (num != 1) {
      if (testFlag && isPrime(num, counter)) return num
      testFlag = false
      while (num % counter == 0) { 
          num /= counter
          testFlag = true
      }
      counter += 2   
    }
    return counter - 2
  }

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution */
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toLong
      println(largestPrimeFactor(n))
    }
  }
}
