object Solution {

  def maxXor(l: Int, r: Int): Int = {
    var result = 0
    for {
      i <- l to r
      j <- i + 1 to r
    } {
      val m = i ^ j
      if (m > result) result = m
    }

    result
  }

  def maxXorS(l: Int, r: Int): Int = {
    var p = l ^ r
    var result = 1
    while (p >= 1) {
      p >>= 1
      result <<= 1
    }
    result - 1
  }

  def main(args: Array[String]) {
    var _l:Int = Console.readInt
    var _r:Int = Console.readInt
    
    val res = maxXor(_l, _r)
    println(res)
  }
}
