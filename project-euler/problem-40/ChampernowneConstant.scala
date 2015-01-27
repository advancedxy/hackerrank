object Solution {

  val headTailTable = ((1 to 18).foldLeft(List((0l, 0l))) { (x, y) =>
    val start = x.head._2 + 1
    val numbers = math.pow(10, y).toLong - math.pow(10, y - 1).toLong
    val end = start + y * numbers - 1
    (start, end) :: x
  }).reverse.toVector

  def digitOfIndex(idx: Long): Int = {
    val n = idx.toString.length
    val (start, _) = headTailTable(n)
    val (idx)
  }
  def main(args: Array[String]) {


  }
}
