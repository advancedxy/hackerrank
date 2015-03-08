object Solution {

  def numOfPalindromes(s: String) = {
    val p = 1e9.toInt + 7
    val chars = Array.fill(256)(0)
    for (c <- s) chars(c) += 1
    var oddNumOfChars = 0
    for (c <- 'a' to 'z') {
      if (chars(c) % 2 != 0) oddNumOfChars += 1
      chars(c) /= 2
    }
    if (oddNumOfChars > 1) return 0L

    def numOfPermutationWithMod(x: Int): Long =
      (1 to x).foldLeft(1L) { _ * _ % p}

    val nums = chars.filter(_ > 0)
    val muls = nums.map { x =>
        val d = numOfPermutationWithMod(x)
        BigInt(d).modPow(p - 2, p).toInt
    }

    muls.foldLeft(numOfPermutationWithMod(nums.sum)) { _ * _ % p }
  }

  def main(args: Array[String]) {
    val s = readLine
    println(numOfPalindromes(s))
  }
}
