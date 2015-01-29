object Solution {

  def reverseTriangleNumber(x: Long): Long = {
    if (x < 1) -1
    else {
      val halfN = math.sqrt(x >> 1).toLong
      if (halfN * (2 * halfN + 1) == x) 2 * halfN
      else if ((halfN + 1) * (2 * halfN + 1) == x) 2 * halfN + 1
      else -1
    }
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val x = readLine.toLong
      println(reverseTriangleNumber(x))
    }
  }
}
