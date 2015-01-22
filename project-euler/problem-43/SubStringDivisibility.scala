object Solution {
  def isDivisible(digits: IndexedSeq[Int], idx: Int) = idx match {
    case 1 => digits(3) % 2 == 0
    case 2 => digits.slice(2, 5).sum % 3 == 0
    case 3 => digits(5) % 5 == 0
    case 4 => digits.slice(4, 7).reduceLeft({ _ * 10 + _ }) % 7 == 0
    case 5 => digits.slice(5, 8).reduceLeft({ _ * 10 + _ }) % 11 == 0
    case 6 => digits.slice(6, 9).reduceLeft({ _ * 10 + _ }) % 13 == 0
    case 7 => digits.slice(7, 10).reduceLeft({ _ * 10 + _ }) % 17 == 0
  }
  def main(args: Array[String]) {
    val n = readLine.toInt
    val nums = (0 to n).permutations
      .filter { x => (0 to n - 3).forall(y => isDivisible(x, y + 1)) }
      .map { x => x.foldLeft(0l) { _ * 10 + _ } }
    println(nums.foldLeft(0l) { _ + _ })
  }
}
