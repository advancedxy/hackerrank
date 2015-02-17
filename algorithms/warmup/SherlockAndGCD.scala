object Solution {
  
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b,  a % b)

  def existsSubsets(arr: Array[Int]): Boolean = {
    val n = arr.size
    for {
      i <- 0 until n
      j <- i + 1 until n
      if arr(i) != arr(j)
    } {
      if (gcd(arr(i), arr(j)) == 1) return true
    }
    if (n == 1 && arr(0) == 1) true else false
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for(_ <- 1 to t) {
      val n = readLine.toInt
      val arr = readLine.split(" ").map(_.toInt)
      if (existsSubsets(arr)) println("YES") else println("NO")
    }
  }
}
