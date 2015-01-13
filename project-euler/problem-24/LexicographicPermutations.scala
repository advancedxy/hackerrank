object Solution {
  case class Cantor(size: Int) {
    if (size > 20) {
      throw new Exception("size too big for permutations, should less than 21")
    }

    private var factor = 1l
    private val factors = 1l +: (for (i <- 1 to size) yield {
      factor = i * factor
      factor
    })

    def revert(index: Long): Seq[Int] = {
      var i = index - 1
      val used = Array.fill(size)(false)

      for (j <- 1 to size) yield {
        val a = i / factors(size - j)
        i = i % factors(size - j)
        val selected = (0 until size).filter(!used(_))(a.toInt)
        used(selected) = true
        selected
      }
    }

    def order(arr: Seq[Int]): Long = {
      assert(arr.toSet.size == size)
      assert(arr.forall(_ < size))
      val used = Array.fill(size)(false)
      val coefficient = arr.map { x =>
        used(x) = true
        (0 until x).filter(!used(_)).size
      }
      1 + coefficient.zipWithIndex.foldLeft(0l) { (x, y) =>
        x + y._1 * factors(size - 1 - y._2)
      }
    }
  }

  def main(args: Array[String]) {
    val word = "abcdefghijklm"
    val cantor = Cantor(word.size)
    val t = readLine.toInt
    for (i <- 1 to t) {
      val order = readLine.toLong
      val indexes = cantor.revert(order)
      println(indexes.map(word(_)).mkString(""))
    }
  }
}
