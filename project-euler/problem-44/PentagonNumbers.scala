object Solution {
  
  def genPentagonNumbers(n: Int): (Array[Long], Set[Long]) = {
    val result = (1l to n).map(x => x * (3 * x - 1) / 2).toArray
    (result, result.toSet)
  }
  
  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    val (penNum, penNumSet) = genPentagonNumbers(n)
    val penNums = (k + 1 until n).filter({ x =>
      val (pn, pk) = (penNum(x), penNum(x - k))
      penNumSet(pn + pk) || penNumSet(pn - pk)
    }).map(penNum(_))
    println(penNums.mkString("\n"))
  }
}
