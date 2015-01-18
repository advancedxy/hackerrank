object Solution {

  def calculateDuplicates(b: Int, n: Int): Array[Int] = {
    val duplicates = for (i <- 2 to n) yield {
      var set = Set.empty[Int]
      for (j <- 1 until i) {
        set ++= (2 to j * b / i).filter(x => (i * x) % j == 0)
      }
      set.size
    }
    (duplicates.foldLeft(List(0)) { case (x, y) =>
      y + x.head :: x
    }).reverse.drop(1).toArray
  }

  def distinctPowers(a: Int, b: Int): Long = {
    var total = 1l * (a - 1) * (b - 1)
    val sqrta = math.sqrt(a).toInt
    val log2a = (math.log(a) / math.log(2)).toInt
    val duplicatesSum = calculateDuplicates(b, log2a)
    val visted = scala.collection.mutable.Set.empty[Int]
    for (i <- 2 to sqrta if !visted(i)) {
      val logia = intLog(a, i)
      total -= duplicatesSum(logia - 2)
      visted ++= (2 to logia).map(math.pow(i, _).toInt)
    }
    total
  }

  def intLog(n: Int, base: Int): Int = {
    var start = 0
    while(math.pow(base, start) <= n) start += 1
    start - 1
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    println(distinctPowers(n, n))
  }
}
