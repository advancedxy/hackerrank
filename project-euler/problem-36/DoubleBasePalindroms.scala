object Solution {

  // Reverse a positive integer, the return integer is a Long number as the
  // reversed integer may bigger than Int.MaxValue
  def reverseInt(n: Int, base: Int): Long = {
    var reversed = 0l
    // n should be greater than 0, otherwise It just returns 0
    var k = n
    
    while (k > 0) {
      reversed = base * reversed + k % base
      k /= base
    }
    reversed
  }

  // Generate palindrom numbers which has n digits.
  def palindromGenerator(n: Int): Seq[Int] = n match {
    case x if x <= 0 => throw new Exception(
      s"n should be greater than 0, but found ${n}")
    case 1 => (1 to 9)
    case 2 => (1 to 9).map(x => x * 10 + x)
    case x if x <= 9=>
      val halfSize = x / 2
      val start = math.pow(10, halfSize - 1).toInt
      val end = 10 * start
      if (x % 2 == 0) 
        (start until end).map(x => x * end + reverseInt(x, 10).toInt)
      else (0 to 9).flatMap { i =>
        (start until end).map(x => (x * 10 + i) * end + reverseInt(x, 10).toInt)
      }
    case 10 =>
      val start = math.pow(10, 4).toInt
      val end = Int.MaxValue / start
      (start to end).map(x => x * start * 10 + reverseInt(x, 10).toInt)
    case _ => throw new Exception(s"n should less than 10, but found ${n}")
  }
  
  def isPalindrom(n: Int, base: Int): Boolean = reverseInt(n, base) == n

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val nDigits = n.toString.length
    val lessThanNDigitsSum = (1 until nDigits).flatMap(palindromGenerator(_)).
      filter({ isPalindrom(_, k) }).sum
    val nDigitsSum = palindromGenerator(nDigits).filter({ x =>
      x < n && isPalindrom(x, k)
    }).sum
    println(lessThanNDigitsSum + nDigitsSum)
  }
}
