object Solution {

    def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

    def mcm(a: Long, b: Long): Long = a * b / gcd(a, b)

    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/  
      val n = readLine.toInt
      for (i <- 1 to n) {
        val num = readLine.toLong
        val m = (1l to num).reduce(mcm)
        println(m)
      }
    }
}
