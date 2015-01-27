object Solution {
  import scala.annotation.tailrec
  import scala.io.Source.stdin

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  // a^2 + b^2 = c^2 => p = a + b +c is even
  // see http://www.mathblog.dk/project-euler-39-perimeter-right-angle-triangle/
  // for more details
  def genCountTable(upperBound: Int): Array[Int] = {
    val result = Array.fill(upperBound + 2)(0)
    var maxCount = 0
    var index = 0
    val sqrt2 = math.sqrt(2)
    val mLimit = math.sqrt(1.0 * upperBound / 2).toInt
    val nLimit = math.sqrt(1.0 * upperBound / 4).toInt

    for {
      n <- 1 to nLimit
      m <- n + 1 to mLimit by 2
      if gcd(m ,n) == 1
    } {
      val p = 2 * m * (m + n)
      for (j <- p to upperBound by p) result(j) += 1
    }

    for (j <- 13 to upperBound + 1 by 2) {
      if (result(j - 1) > maxCount) {
        maxCount = result(j - 1)
        index = j - 1
        result(j) = index
      }
      else {
        result(j) = index
      }
    }
    result
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    val tests = (1 to t).map(x => readLine.toInt)
    val n = tests.max
    val countTable = genCountTable(n)
    tests.foreach { x =>
    println(countTable(x / 2 * 2 + 1))}
  }
}
