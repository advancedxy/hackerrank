object Solution {
  
  def isFibo(n: Long): Boolean = {
    val sqrt5 = math.sqrt(5)
    val theta = (1 + sqrt5) / 2
    val index = (math.log(n * sqrt5 + 0.5) / math.log(theta)).toLong
    val fibOfIndex = (math.pow(theta, index) / sqrt5 + 0.5).toLong
    fibOfIndex == n
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val n = readLine.toLong
      println(if (isFibo(n)) "IsFibo" else "IsNotFibo")
    }
  }
}
