object Solution {

  def insertionSort(nums: Array[Int], n: Int) {
    val v = nums(n - 1)
    var i = n - 2
    while (i >= 0 && nums(i) > v) {
      nums(i + 1) = nums(i)
      println(nums.mkString(" "))
      i -= 1
    }
    nums(i + 1) = v
    println(nums.mkString(" "))
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val nums = readLine.split(" ").map(_.toInt)
    insertionSort(nums, n)
  }
}
