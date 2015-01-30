object Solution {

  // this code won't work for large p. if p ~ Long.MaxValue and a ~ p, b ~ p,
  // the ma * limit % p * num % p ~ ma * mb % p where ma * mb will overflow.
  // ~ means left is close to but than than right
  def modMulLong(a: Long, b: Long, p: Long): Long = {
    if (a == 0 || b == 0) return 0
    val ma = a % p
    val mb = b % p
    val limit = Long.MaxValue / ma
    if (limit > mb) ma * mb % p
    else {
      val (num, rem) = (mb / limit, mb % limit)
      (ma * limit % p * num % p + ma * rem % p) % p
    }
  }

  def lastTenDigits(n: Int): Long = {
    val moduloP = 1e10.toLong
    var result = 1L
    var exp = n
    var base = n.toLong
    while (exp > 0) {
      if (base == 0) return 0
      else {
        if ((exp & 1) == 1) result = modMulLong(base, result, moduloP)
        exp >>= 1
        base = modMulLong(base, base, moduloP)
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
