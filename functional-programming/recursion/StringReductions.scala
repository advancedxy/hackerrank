object Solution {
  import scala.collection.mutable.BitSet

  def stringReduction(s: String): String = {
    val charSet = BitSet.empty
    for (c <- s if !charSet(c)) yield {
      charSet += c
      c
    }
  }

  def main(args: Array[String]) {
    val s = readLine
    println(stringReduction(s))
  }
}
