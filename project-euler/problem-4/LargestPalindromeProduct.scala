object Solution {

  def intToDigits(n: Int): List[Int] = ("" + n).map(_ - 48).toList

  def digitsToInt(ls: Seq[Int]) = ls.foldLeft(0) {
    case (acc, digit) => acc * 10 + digit
  }
  def palindromesUnderN(n: Int): List[Int] = {
    val digits = intToDigits(n)

    val largestPalindrome: List[Int] = digits.zip(digits.reverse).map {
      case (x, y) => math.max(x, y) 
    }

    val largestNum = digitsToInt(largestPalindrome)
    val half = digitsToInt(largestPalindrome.take(digits.length / 2))
    (for (i <- (half to 101 by -1).toList) yield {
      val ds: List[Int] = intToDigits(i)
      val even: List[Int] = (ds ++ ds.reverse)
      val odd:List[List[Int]] = for(j <- (9 to 0 by -1).toList) yield
                                ds ++ (j :: ds.reverse)
      even :: odd
    }).flatten.filter( x => x.length <= digits.length).map(digitsToInt(_)).filter(_ <= n)
  }

  def largestPalindrome(n: Int) = palindromesUnderN(n).filter(x => (101 to 999).exists(y => x % y == 0 && (x / y).toString.length == 3)).max

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution */
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toInt
      println(largestPalindrome(n))
    }
  }
}
