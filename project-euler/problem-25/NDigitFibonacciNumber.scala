object Solution {
  
  // see http://en.wikipedia.org/wiki/Fibonacci_number for more details
  // math rocks
  val theta = (1 + math.sqrt(5)) / 2
  val logTheta = math.log(theta)
  val l1 = math.log(math.sqrt(5)) / logTheta
  val l2 = math.log(10) / logTheta
  def indexOfFib(digits: Int): Int = {
    math.ceil(l1 + (digits - 1) * l2).toInt
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for(i <- 1 to t) {
      val digits = readLine.toInt
      println(indexOfFib(digits))
    }
  }

}
