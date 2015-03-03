object Solution {
  import scala.collection.mutable.{HashMap, Queue}

  def choices(flavors: Array[Int], m: Int): Seq[Int] = {
    val indexMap = new HashMap[Int, Queue[Int]]() {
      override def default(x: Int) = Queue[Int]()
    }

    for (i <- 0 until flavors.size)
      indexMap(flavors(i)) = indexMap(flavors(i)) :+ i

    for {
      i <- 0 until flavors.size
      j <- indexMap(m - flavors(i)) if j > i
      idx <- Array(i, j)
    } yield idx + 1
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (_ <- 1 to t) {
      val m = readLine.toInt
      val n = readLine.toInt
      val flavors = readLine.split(" ").map(_.toInt)
      println(choices(flavors, m).mkString(" "))
    }
  }
}
