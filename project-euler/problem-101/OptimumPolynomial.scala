object Solution {
  val prime = 1e9.toInt + 7

  class Polynomial(coefficients: Array[Long]) {
    val degree = coefficients.length - 1

    def get(i: Int) = coefficients(i)

    def evaluate(x: Int) = {
      var result = 0L
      var i = degree
      while (i >= 0) {
        result = (result * x + coefficients(i)) % prime
        i -= 1
      }

      result
    }
  }

  def main(args: Array[String]) = {
    val n = readInt
    // A_i can be 2^31
    val coefficients = readLine.split(" ").map(_.toLong).toArray
    val poly = new Polynomial(coefficients)
    val p = Array.fill[Long](n)(0L)
    // Cache prime division result. It's too expensive to use BigInt
    val primeModule = (for (i <- 0 to n) yield BigInt(i).modPow(prime - 2, prime).longValue).toArray
    val fits = Array.fill(n)(0L)
    assert(n + 1 == coefficients.length)
    var u = poly.evaluate(1)
    var i = 1
    while (i <= n) {
      p(i - 1) = i * u % prime
      var s = p(i - 1)
      var j = 1
      while (j < i) {
        p(j - 1) = (((prime - p(j - 1)) % prime * i % prime) * primeModule(i + 1 -j)) % prime
        s = (s + p(j - 1)) % prime
        j += 1
      }
      u = poly.evaluate(i + 1)
      fits(i - 1) = s
      i += 1
    }
    println(fits.mkString(" "))
  }
}
