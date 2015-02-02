object Solution {
  import java.math.BigInteger

  // This code came from
  // http://stackoverflow.com/questions/18828377/biginteger-count-the-number-of-decimal-digits-in-a-scalable-method
  def log10(bigNumber: BigInteger): Int = {
    var digits = 0
    var huge = bigNumber
    var bits = huge.bitLength()
    // Serious reductions.
    while (bits > 4) {
      // 4 > log[2](10) so we should not reduce it too far.
      val reduce = bits / 4;
      // Divide by 10^reduce
      huge = huge.divide(BigInteger.TEN.pow(reduce));
      // Removed that many decimal digits.
      digits += reduce;
      // Recalculate bitLength
      bits = huge.bitLength();
    }
    // Now 4 bits or less - add 1 if necessary.
    if ( huge.intValue() > 9 ) {
      digits += 1;
    }
    return digits;
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    var (num, den) = (BigInt(1393), BigInt(185))
    for (i <- 8 to n) {
      val nc = log10(num.bigInteger)
      val dc = log10(den.bigInteger)
      if (nc - dc > 0) println(i)
      num = num + 2 * den
      den = num - den
    }
  }
}
