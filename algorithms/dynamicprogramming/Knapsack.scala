object Solution {

  def packedSum(nums: Array[Int], k: Int): Int = {
    val choices = Array.fill(k + 1)(false)
    for (num <- nums if num <= k) choices(num) = true
    val min = (1 to k).find(i => choices(i))
    if (min.isEmpty) 0
    else {
      val minNum = min.get
      if (minNum == 1 || minNum == k) k
      else {
        val modulos = Array.fill(minNum)(Int.MaxValue)
        modulos(0) = minNum
        for (i <- minNum + 1 to k if choices(i)) {
          var c = 1
          while (c < minNum && c * i <= k) {
            val p = c * i % minNum
            modulos(p) = modulos(p) min (c * i)
            c += 1
          }
        }

        val p = k % minNum
        val start = k - p
        var result = start
        for (j <- 0 to p if modulos(j) <= k) result = start + j
        result
      }
    }
  }

  def main(args: Array[String]) {
    val t = readInt
    for(_ <- 1 to t) {
      val Array(n, k) = readLine.split(" ").map(_.toInt)
      val nums = readLine.split(" ").map(_.toInt)
      println(packedSum(nums, k))
    }
  }
}
