object Solution {

  def finalNumbers(n: Int, a: Int, b: Int) = {
    val min = a min b
    val max = a max b
    val diff = max - min
    min * (n - 1) to max * (n - 1) by diff
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val (n, a, b) = (readLine.toInt, readLine.toInt, readLine.toInt)
      println(finalNumbers(n, a, b).mkString(" "))
    }
  }
}
