object Solution {
  import scala.collection.mutable

  // the largestgem 
  def largestPem(x: Long): Long = {
    var digits = Array.fill(10)(0)
    var temp = x
    var returnVal = 0L
    while (temp > 0) {
      digits((temp % 10).toInt) += 1
      temp /= 10
    }
    for (j <- 9 to 0 by -1) {
      while(digits(j) > 0) {
        returnVal = returnVal * 10 + j
        digits(j) -= 1
      }
    }
    returnVal
  }

  def genKCubes(n: Int, k: Int) = {
    val cache = new mutable.HashMap[Long, (Int, Long)] {
      override def default(k: Long) = (0, 0L)
    }

    for (i <- 345L until n) { // L will be Long, so cube won't overflow
      val cube = i * i * i
      val key = largestPem(cube)
      val (count, smallest) = cache(key)
      cache(key) = (count + 1, if (smallest == 0) cube else smallest)
    }
    cache.map(_._2).filter(_._1 == k).map(_._2)
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val cubes = genKCubes(n, k).toList.sorted
    cubes.foreach { x =>
      println(x)
    }
  }
}
