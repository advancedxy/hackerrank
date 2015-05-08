object Solution {

  def squareRoot(i: Int, nDigits: Int): BigInt = {
    val limit = BigInt(10).pow(nDigits + 1)
    var a = 5 * BigInt(i)
    var b = BigInt(5)
    while (b < limit) {
      if (a >= b) {
        a -= b
        b += 10
      }
      else {
        a *= 100
        b = (b / 10) * 100 + 5
      }
    }
    b / 100
  }

  def isPerfectRoot(x: Int): Boolean = {
    val sqrt = math.sqrt(x).toInt
    sqrt * sqrt == x
  }

  def main(args: Array[String]) {
    val n = readInt
    val p = readInt
    val sum = (1 to n).filter(!isPerfectRoot(_))
                      .map(x => squareRoot(x, p).toString.map(_.asDigit).sum).sum
    println(sum)
  }
}
