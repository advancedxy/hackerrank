object Solution {

  def main(args: Array[String]) {
    val s = readLine
    val charSet = (for (c <- s.toLowerCase if c != ' ') yield c).toSet
    if (charSet.size == 26) {
      println("pangram")
    } else {
      println("not pangram")
    }
  }
}
