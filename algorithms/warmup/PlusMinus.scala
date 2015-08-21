object Solution {

  def main(args: Array[String]) = {
    val _ = readInt
    val ns = readLine.split(" ").map(_.toInt)
    val nc = Array.fill(3)(0)
    for (n <- ns) {
      val i = n match {
        case _ if n > 0 => 0
        case 0 => 2
        case _ => 1
      }
      nc(i) += 1
    }
    val total = nc.sum
    for (i <- nc) {
      println(1D * i / total)
    }
  }
}
