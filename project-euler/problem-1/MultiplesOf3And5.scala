object Solution {
  def sumOfMultiples(n: Int): Long = {
    def sum(m: Int): Long = {
      val size = ((n -1 ) / m).toLong
      return m * size * (size + 1) / 2
    }
    sum(3) + sum(5) - sum(3*5)
  }

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution */
    val m = readLine.toInt
    for (i <- 1 to m) {
      val n = readLine.toInt
      println(sumOfMultiples(n))
    }
  }
}
