object Solution {

  def moves(n: BigInt): Int = {
    var c = 0
    var num = n
    while (num > 1) {
      val bitLength = num.bitLength
      val nearPowerOf2 = BigInt(1) << (bitLength - 1)
      val rem = num - nearPowerOf2
      c += 1
      if (rem == 0) num = num >> 1
      else num = rem
    }
    c
  }

  def main(args: Array[String]) {
    for (_ <- 1 to readInt) {
      val n = BigInt(readLine)
      if (moves(n) % 2 == 0) println("Richard") else println("Louise")
    }
  }
}
