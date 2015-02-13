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

  def main(args: Array[String]) {
    var _l:Int = Console.readInt
    var _r:Int = Console.readInt
    
    val res = maxXor(_l, _r)
    println(res)
  }
}
