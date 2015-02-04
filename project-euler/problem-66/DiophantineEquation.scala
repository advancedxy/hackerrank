object Solution {

  def findDUnderN(n: Int): Int = {
    val isSquareTable = Array.fill(n + 1)(false)
    
    var result = 0
    var maxX = BigInt(0)
    for (D <- 2 to n) {
      val sqrtD = math.sqrt(D).toInt
      if (sqrtD * sqrtD != D) {
        var (m, d, a) = (BigInt(0), BigInt(1), BigInt(sqrtD))
        var num1 = BigInt(1)
        var num = a
        var den1 = BigInt(0)
        var den = BigInt(1)
        while (num * num - D * den * den != 1) {
          m = d * a - m
          d = (D - m * m) / d
          a = (sqrtD + m) / d
          var num2 = num1
          num1 = num
          var den2 = den1
          den1 = den
 
          num = a * num1 + num2;
          den = a * den1 + den2;
        }
        if(num > maxX) {
          maxX = num
          result = D
        }
      }
    }
    result
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    println(findDUnderN(n))
  }
}
