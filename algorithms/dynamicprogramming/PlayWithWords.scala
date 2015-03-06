object Solution {

  def maxProduct(s: String): Int = {
    
    val n = s.size
    (0 to n / 2).map(x => largestPalSeq(0, x) * largestPalSeq(x + 1, n - 1)).max
  }

  def main(args: Array[String]) {

    val s = readLine
    println(maxProduct(s))
  }
}
