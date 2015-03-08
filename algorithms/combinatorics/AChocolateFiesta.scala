object Solution {

  def evenSubsets(even: Int, odd: Int) = {
    val p =1e9.toInt + 7
    (BigInt(2).modPow(even + math.max(0, odd - 1), p) - 1) % p
  }

  def main(args: Array[String]) {
    val _ = readLine
    val nums = readLine.split(" ").map(_.toInt)
    val even = nums.filter(_ % 2  == 0).size
    val odd = nums.size - even
    println(evenSubsets(even, odd))
  }
}
