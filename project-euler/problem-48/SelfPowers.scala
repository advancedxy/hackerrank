object Solution {

  def lastTenDigits(n: Int): Long = {
    val moduloP = 1e10.toLong
    var result = 1L
    var exp = n
    var base = n.toLong
    while (exp > 0) {
      if (base == 0) return 0
      else {
        val nLimit = Long.MaxValue / base
        if ((exp & 1) == 1) {
          if (nLimit > result) result = (result * base) % moduloP
          else {
            val num = result / nLimit
            val remain = result - nLimit * num
            result = ((base * nLimit) % moduloP * num + (base * remain) % moduloP) % moduloP
          }
        }
        exp >>= 1
        if (nLimit > base) base = (base * base) % moduloP
        else {
          val num = base /nLimit
          val remain = base - nLimit * num
          base = ((base * nLimit) % moduloP * num + (base * remain) % moduloP) % moduloP
        }
      }
    }
    result
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    var digits = 0L
    var i = 1
    while (i <= n) {
      digits += lastTenDigits(i)
      i += 1
    }
    println(digits % 1e10.toLong)
  }
}
