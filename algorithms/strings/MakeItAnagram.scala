object Solution {

  def deletionForAnagram(s: String, t: String): Int = {
    val sc,tc = Array.fill(256)(0)
    for (c <- s) sc(c) += 1
    for (c <- t) tc(c) += 1
    sc.zip(tc).map(x => math.abs(x._1 - x._2)).sum
  }

  def main(args: Array[String]) {
    val s = readLine
    val t = readLine
    println(deletionForAnagram(s, t))
  }
}
