object Solution {

  def getCutsForSticks(sticks: Array[Int]): Seq[Int] = {
    val n = sticks.size
    val upperBound = 1000 // the upperBound for a_i in the sticks array.
    val counts = Array.fill(upperBound + 1)(0)
    for (s <- sticks) counts(s) += 1
    var result = n
    for (c <- counts if c > 0 && result > 0) yield {
      result = result - c
      result + c
    }
  }

  def main(args: Array[String]) {
    val _ = readLine
    val sticks = readLine.split(" ").map(_.toInt)
    val cuts = getCutsForSticks(sticks)
    println(cuts.mkString("\n"))
  }
}
