object Solution {

  def numberOfChanges(str: String): Int = {
    val n = str.length
    if (n % 2 == 1) -1
    else {
      val s = str.substring(0, n / 2)
      val t = str.substring(n / 2)
      val sc, tc = Array.fill(256)(0)
      for (c <- s) sc(c) += 1
      for (c <- t) tc(c) += 1
      sc.zip(tc).map(x => math.abs(x._1 - x._2)).sum / 2
    }
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val str = readLine
      println(numberOfChanges(str))
    }
  }
}
