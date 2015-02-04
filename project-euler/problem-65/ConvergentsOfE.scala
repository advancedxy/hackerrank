object Solution {
  
  def numeratorForE(n: Int): BigInt = {
    // n_k = a_k * n_(k-1) + n_(k-2)
    require(n > 0)
    var (a, b) = (BigInt(2), BigInt(3))
    if (n == 1) a
    else if (n == 2) b
    else {
      for (j <- 3 to n) {
        val temp = a
        val c = if (j % 3 == 0) 2 * (j / 3) else 1
        a = b
        b = c * a + temp
      }
      b
    }
  }

  def sumOfDigits(x: BigInt): Int = x.toString.map(_.asDigit).sum

  def main(args: Array[String]) {
    val n = readLine.toInt
    val numerator = numeratorForE(n)
    println(sumOfDigits(numerator))
  }
}
