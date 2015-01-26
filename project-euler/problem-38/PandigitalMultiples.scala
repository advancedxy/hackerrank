object Solution {

  def genMultiples(n: Int, k: Int): Seq[Int] = {
    val upperBound = 1e5.toInt
    val limit = if (k == 8) upperBound / 2 else upperBound

    def canBePandigitalMultiples(m: Int): Boolean = {
      val digitAppeared = Array.fill(9 + 1)(false)
      // set 0 to has be appeared. If we see 0 again, we can just return false
      digitAppeared(0) = true 
      if (k == 8) digitAppeared(9) = true
      var count = 0 // the number of digits has be appeared.
      var mb = 1
      while (count < k) {
        var mm = mb * m
        while (mm > 0) {
          val digit = mm % 10
          if (digitAppeared(digit)) return false
          else {
            digitAppeared(digit) = true
            count += 1
            mm /= 10
          }
        }
        mb += 1
      }
      true
    }

    for {
      m <- 2 to math.min(n, limit)
      if canBePandigitalMultiples(m)
    } yield m
  }
  
  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    println(genMultiples(n, k).mkString("\n"))
  }
}
