object Solution {

  def canBePalindrome(s: String): Boolean = {
    val charCount = Array.fill(256)(0)
    for (c <- s) charCount(c) += 1
    charCount.filter(_ % 2 != 0).size <= 1
  }

  def main(args: Array[String]) {
    val s = readLine
    println(if (canBePalindrome(s)) "YES" else "NO")
  }
}
