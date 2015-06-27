object Solution {

  def correctRomanNumeral(s: String): String = {
    val symbols = "IVXLCDM".toList
    val numeral = List(1, 5, 10, 50, 100, 500, 1000)
    val symbolMap = symbols.zip(numeral).toMap

    def str2Int(s: String): Int = {
      var r = symbolMap(s(0))
      for (i <- 1 until s.size) {
        val s1 = symbolMap(s(i - 1))
        val s2 = symbolMap(s(i))
        if (s2 > s1) r += -2 * s1 + s2
        else r += s2
      }
      r
    }

    def int2RomanNumeral(i: Int): String = {
      def helper(i: Int, acc: List[Char]): List[Char] = i match {
        case i if i >= 0 && i <= 3 => ("I" * i).toList ::: acc
        case 4 => "IV".toList.reverse ::: acc
        case 5 => 'V' :: acc
        case i if i >= 6 && i <= 8 =>
          ('V' + "I" * (i - 5)).toList.reverse ::: acc
        case 9 => "IX".toList.reverse ::: acc
        case 10 => 'X' :: acc
        case i if i > 10 && i < 40 =>
          val n = i / 10
          val r = i % 10
          helper(r, ("X" * n).toList ::: acc)
        case 40 => "XL".toList.reverse ::: acc
        case i if i > 40 && i < 50 =>
          val r = i - 40
          helper(r, "XL".toList.reverse ::: acc)
        case i if i >= 50 && i < 90 => helper(i - 50, 'L' :: acc)
        case i if i >= 90 && i < 100 =>
          helper(i - 90, "XC".toList.reverse ::: acc)
        case 100 => 'C' :: acc
        case i if i > 100 && i < 400 =>
          val n = i / 100
          val r = i % 100
          helper(r, ("C" * n).toList.reverse ::: acc)
        case i if i >= 400 && i < 500 =>
          helper(i - 400, "CD".toList.reverse ::: acc)
        case i if i >= 500 && i < 900 =>
          helper(i - 500, 'D' :: acc)
        case i if i >= 900 && i < 1000 =>
          helper(i - 900, "CM".toList.reverse ::: acc)
        case i if i >= 1000 =>
          val n = i / 1000
          val r = i % 1000
          helper(r, ("M" * n).toList ::: acc)
      }

      helper(i, Nil).reverse.mkString
    }
    int2RomanNumeral(str2Int(s))
  }

  def main(args: Array[String]) {
    val t = readInt
    for (i <- 1 to t) println(correctRomanNumeral(readLine.trim))
  }
}
