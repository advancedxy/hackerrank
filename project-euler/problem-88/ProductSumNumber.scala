object Solution {

  def productSumNumber(n: Int): Long = {
    // For k size product sum, the minimal n will be greater than k as there are
    // k integers. The max will be less than 2 * k as we can {2, k} and k -2
    // ones to get 2 * k
    val kmax = n
    val result = Array.fill(kmax + 1)(2 * kmax)

    def prodSumHelper(prod: Int, sum: Int, c: Int, start: Int) {
      val k = prod - sum + c // product - sum + number of factors
      if (k <= kmax) {
        if (prod < result(k)) result(k) = prod
        for (i <- start to kmax/prod * 2)
          prodSumHelper(prod * i, sum + i, c + 1, i)
      }
    }

    prodSumHelper(1, 1, 1, 2)
    result.slice(2, n + 1).toSet.foldLeft(0L) { _ + _ }
  }

  def main(args: Array[String]) {
    val n = readInt
    print(productSumNumber(n))
  }
}
