object Solution {

  def main(args: Array[String]) = {
    val n = readInt
    for (i <- 1 to n) {
      println(" " * (n - i) + "#" * i)
    }
  }
}
