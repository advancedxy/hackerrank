object Solution {

  def minPathSum3(matrix: Array[Array[Int]]): Long = {
    val n = matrix.size
    val minSumMatrix = Array.fill(n)(Array.fill(n)(0L))
    // set the first column.
    for (i <- 0 until n) {
      minSumMatrix(i)(0) = matrix(i)(0)
    }

    // calculate all the other columns
    for (i <- 1 until n) {
      // traverse down
      minSumMatrix(0)(i) = matrix(0)(i) + minSumMatrix(0)(i - 1)
      for (j <- 1 until n) {
        minSumMatrix(j)(i) =
          (minSumMatrix(j)(i - 1) min minSumMatrix(j - 1)(i)) + matrix(j)(i)
      }

      // traverse up
      for (j <- n - 2 to 0 by -1) {
        minSumMatrix(j)(i) =
          minSumMatrix(j)(i) min (minSumMatrix(j + 1)(i) + matrix(j)(i))
      }
    }

    (0 until n).map(minSumMatrix(_)(n-1)).min
  }

  def main(args: Array[String]) {
    val n = readInt
    val matrix = (1 to n).toArray.map(x => readLine.split(" ").map(_.toInt))
    println(minPathSum3(matrix))
  }
}
