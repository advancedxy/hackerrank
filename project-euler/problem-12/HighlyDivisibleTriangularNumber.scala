object Solution {
  val upperBound = 1001
  val indexes = Array.fill(upperBound + 1)(0)

  def numberOfFactors(n: Long): Int = {
    // consider n*(n+1)/2, only n or n+1 is divided by 2. And the number of
    // factors for n*(n+1)/2 is numberOfFactors(n/2) * numberOfFactors(n+1)
    // assuming n is divided by 2. So this function is slightly different from
    // the original.
    val number= if (n % 2 == 0) n / 2 else n
    var p = 1l;
    var count = 0
    while(p * p < number) {
      if (number % p == 0) count += 1
      p += 1
    }
    if (p * p == number) 2 * count + 1 else 2 * count
  }

  def caculateIndexes {
    var iterateIndex = 1
    indexes(iterateIndex) = 1
    var n = 1
    var leftFactors, rightFactors = 1
    while (iterateIndex <= upperBound) {
      leftFactors = rightFactors
      rightFactors = numberOfFactors(n + 1)
      val nFactors = leftFactors * rightFactors
      if (nFactors > iterateIndex) {
        for (i <- iterateIndex + 1 to math.min(nFactors, upperBound)) indexes(i) = n
        iterateIndex = nFactors
      }
      n += 1
    }
  }

  def main(args: Array[String]) {
    caculateIndexes
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      val nth = indexes(n + 1)
      println(1l * nth * (nth + 1) / 2)
    }
  }
}
