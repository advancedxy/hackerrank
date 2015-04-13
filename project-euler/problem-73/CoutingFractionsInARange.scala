object Solution {

  // I found this pdf is quite useful
  // https://nrciz.googlecode.com/svn/trunk/projecteuler/pdfs/073_overview.pdf
  // I add this pdf to my repo in case googlecode is dead
  // Will update the code when I get some time.

  def main(args: Array[String]) {
    val Array(a, d) = readLine.split(" ").map(_.toInt)
    println(countsInRangeStack(a, d))
  }
}
