object Solution {

  def palindromeIndex(s: String): Int = {
    def isPalindrome(s: String): Boolean = s == s.reverse
    if (isPalindrome(s)) return -1
    val n = s.length
    val srev = s.reverse
    var i = 0
    while (s(i) == srev(i)) i += 1
    // remove i
    val ns = s.substring(0, i) + s.substring(i + 1)
    if (isPalindrome(ns)) i else n - 1 - i
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val s = readLine
      println(palindromeIndex(s))
    }
  }
}
