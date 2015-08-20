object Solution {

  def diagonalDiff(matrix: Array[Array[Int]]): Int = {
    val n = matrix.size
    val diagonalDiff = (0 until n).map(x => matrix(x)(x) - matrix(x)(n-1-x)).sum
    return math.abs(diagonalDiff)
  }

  def main(args: Array[String]) = {
    val n = readInt
    val matrix = Array.fill(n)(Array.empty[Int])
    for (i <- 0 until n) {
      val row = readLine.split(" ").map(_.toInt)
      matrix(i) = row
    }
    println(diagonalDiff(matrix))
  }
}
