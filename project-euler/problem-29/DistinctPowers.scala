object Solution {

  def distinctPowers(a: Int, b: Int): Long = {
    var total = 1l * (a - 1) * (b - 1)
    val sqrta = math.sqrt(a).toInt
    val log2a = (math.log(a) / math.log(2)).toInt
    val baseSet = (2 to b).toSet
    var set = baseSet
    val duplicates = for (i <- 2 to log2a) yield {
      val newSet = baseSet.map(_ * i)
      val previousCount = set.size
      set = set ++ newSet
      previousCount + newSet.size - set.size
    }
    val duplicatesSum = (duplicates.foldLeft(List(0)) { case (x, y) =>
      y + x.head :: x
    }).reverse.drop(1).toVector
    val visted = scala.collection.mutable.Set.empty[Int]
    for (i <- 2 to sqrta if !visted(i)) {
      val logia = (math.log(a) / math.log(i)).toInt
      total -= duplicatesSum(logia - 2)
      visted ++= (2 to logia).map(math.pow(i, _).toInt)
    }
    total
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    println(distinctPowers(n, n))
  }
}
