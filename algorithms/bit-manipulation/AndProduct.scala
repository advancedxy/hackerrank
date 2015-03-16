object Solution {

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val Array(l, r) = readLine.split(" ").map(_.toLong)
      val ls = l.toBinaryString
      val rs = r.toBinaryString
      if (ls.length != rs.length) println(0)
      else {
        val prefix = ls.zip(rs)
                       .takeWhile(x => x._1 == x._2)
                       .map(x => x._1).mkString
        println(java.lang.Long.parseLong(prefix.toString, 2) << (ls.length - prefix.length))
      }
    }
  }
}
