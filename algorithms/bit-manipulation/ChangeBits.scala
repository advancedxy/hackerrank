object Solution {

  def main(args: Array[String]) {
    val Array(n, q) = readLine.split(" ").map(_.toInt)
    var a = BigInt(readLine, 2)
    var b = BigInt(readLine, 2)
    var result = List.empty[Int]
    for (_ <- 1 to q) {
      val head :: tail = readLine.split(" ").toList
      head match {
        case "set_a" =>
          val List(idx, x) = tail.map(_.toInt)
          a = if (x == 1) a.setBit(idx) else a.clearBit(idx)
        case "set_b" =>
          val List(idx, x) = tail.map(_.toInt)
          b = if (x == 1) b.setBit(idx) else b.clearBit(idx)
        case "get_c" =>
          val idx = tail.head.toInt
          result = if ((a + b).testBit(idx)) 1 :: result else 0 :: result
      }
    }

    println(result.reverse.mkString)
  }
}
