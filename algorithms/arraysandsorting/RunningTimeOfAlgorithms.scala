object Solution {

  def insertionSort(nums: Array[Int]) {
    val n = nums.size
    var numOfShifts = 0
    for (i <- 1 until n) {
      var j = i
      while (j >= 1 && nums(j) < nums(j - 1)) {
        numOfShifts += 1
        val temp = nums(j)
        nums(j) = nums(j - 1)
        nums(j - 1) = temp
        j -= 1
      }
    }
    println(numOfShifts)
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    insertionSort(nums)
  }
}
