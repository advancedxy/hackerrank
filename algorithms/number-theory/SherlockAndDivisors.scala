object Solution {

  def numberOfDivisors(n: Int): Int = {
    var ans = 1
    var p = 2
    var nn = n
    while (p <= nn) {
      if (p * p > nn) p = nn // nn is a prime, we don't need to check further.
      var e = 1
      while (nn % p == 0) {
        nn /= p
        e += 1
      }
      ans *= e
      p += 1
    }
    ans
  }


  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val n = readInt
      if (n % 2 == 0) println(numberOfDivisors(n / 2))
      else println(0)
    }
  }
}
