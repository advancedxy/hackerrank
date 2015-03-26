object Solution {

  def nums(n: Int, m: Int): Long = n match {
    case 1 => m
    case 2 => m * (m - 1)
    case x =>
      val pre = m * (m - 1)
      if (pre == 0) 0
      else
        (pre * BigInt(m - 2).modPow(n - 2, 1e9.toInt + 7)).mod(1e9.toInt + 7).toInt
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(n, m) = readLine.split(" ").map(_.toInt)
      println(nums(n, m))
    }
  }
}
