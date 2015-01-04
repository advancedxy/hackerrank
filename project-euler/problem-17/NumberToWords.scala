object Solution {
  val numberWordMapUnderTwenty =
    Vector("Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven",
           "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen",
           "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen")
  
  val numberWordMapOfTens = 
    Vector("Zero", "Ten", "Twenty", "Thirty", "Forty", "Fifty", "Sixty",
           "Seventy", "Eighty", "Ninety")

  val numberWordMapOfThousands = Vector("Thousand", "Million", "Billion")

  def numberToWords(n: Long): String = n match {
    case x if x < 20 => numberWordMapUnderTwenty(x.toInt)
    case x if x < 100 =>
      val (tens, modulo) = (x.toInt / 10, x.toInt % 10)
      if (modulo != 0) numberWordMapOfTens(tens) + " " + numberToWords(modulo)
      else numberWordMapOfTens(tens)
    case x if x < 1000 =>
      val (hundreds, modulo) = (x.toInt / 100, x.toInt % 100)
      if (modulo != 0)
        numberWordMapUnderTwenty(hundreds) + " Hundred " + numberToWords(modulo)
      else
        numberWordMapUnderTwenty(hundreds) + " Hundred"
    case x =>
      val (numberUnderThousands, larger) = x.toString.reverse.grouped(3)
                                            .map(_.reverse.toInt).toVector
                                            .splitAt(1)

      val largerWords =
        larger.zip(numberWordMapOfThousands)
              .reverse
              .filter(x => x._1 != 0)
              .map(x => numberToWords(x._1) + " " + x._2)
              .mkString(" ")
      val small = numberUnderThousands(0)
      if (small != 0) largerWords + " " + numberToWords(small)
      else largerWords
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val n = readLine.toLong
      println(numberToWords(n))
    }
  }
}
