object Solution {

  def fullCountingSort(lines: Seq[(Int, String)]): Array[String] = {
    val size = lines.size
    val result = Array.fill(size)("")
    val nums = lines.map(_._1)
    val occCount = Array.fill(100)(0)
    for (num <- nums) occCount(num) += 1
    var sum = 0
    val countSum = for (c <- occCount) yield {
      sum += c
      sum
    }
    var i = 0
    for ((n, string) <- lines) {
      val index = countSum(n) - occCount(n)
      occCount(n) -= 1
      i += 1
      if (i <= size / 2) result(index) = "-"
      else result(index) = string
    }
    result
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val lines = (1 to n).map { x =>
      val Array(num, s) = readLine.split(" ")
      (num.toInt, s)
    }
    println(fullCountingSort(lines).mkString(" "))
  }
}
