object Solution {

  def isPermuNums(m: Int, n: Int): Boolean = {
    if (m == n) return true
    val digits = Array.fill(10)(0)
    var temp = m
    while(temp > 0) {
      digits(temp % 10) += 1
      temp /= 10
    }
    temp = n
    while(temp > 0) {
      digits(temp % 10) -= 1
      temp /= 10
    }
    for (c <- digits) {
      if (c != 0) return false
    }
    true
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val digits = n.toString.map(_.asDigit)
    val slots = (1 to k)
    for (c <- 6 to digits.size) {
      val start = math.pow(10, c - 1).toInt
      val end = (math.pow(10, c).toInt / k) min n
      for (j <- start to end) {
        if (slots.forall(x => isPermuNums(x * j, j))) 
          println(slots.map(_ * j).mkString(" "))
      }
    }
  }
}
