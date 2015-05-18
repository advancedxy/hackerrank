object Solution {

  def main(args: Array[String]) {
    val n = readInt
    val matrix = (1 to n).map(x => readLine.split(" ").map(_.toInt))
    val pathSumMinimum = Array.fill(n)(Array.fill(n)(0L))
    var sum = 0L
    for (i <- 0 until n) {
      sum += matrix(0)(i)
      pathSumMinimum(0)(i) = sum
    }
    sum = matrix(0)(0)
    for (i <- 1 until n) {
      sum += matrix(i)(0)
      pathSumMinimum(i)(0) = sum
    }
    for { i <- 1 until n
          j <- 1 until n
    }
    pathSumMinimum(i)(j) = matrix(i)(j) +
      math.min(pathSumMinimum(i - 1)(j), pathSumMiminum(i)(j - 1))

    println(pathSumMinimum(n - 1)(n - 1))
  }
}
