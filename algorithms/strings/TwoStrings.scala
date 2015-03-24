object Solution {

  def hasCommon(s: String, t: String): Boolean = {
    (s.toSet & t.toSet).size >= 1
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val s = readLine
      val t = readLine
      if (hasCommon(s, t)) println("YES") else println("NO")
    }
  }
}
