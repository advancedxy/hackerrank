object Solution {
  
  // see http://mathforum.org/library/drmath/view/62324.html for more details.
  def wday(year: Long, month: Int, day: Int): Int = {
    var adjustment, mm, yy = 0l
    adjustment = (14 - month) / 12
    mm = month + 12 * adjustment - 2
    yy = year - adjustment

    val w = (day + (13 * mm - 1) / 5 +
        yy + yy / 4 - yy / 100 + yy / 400) % 7
    w.toInt
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val Array(y1, m1, d1) = readLine.split(" ").map(_.toLong)
      val Array(y2, m2, d2) = readLine.split(" ").map(_.toLong)

      val sundays = (for {
        year <- y1 to y2
        m <- 1 to 12
        if wday(year, m, 1) == 0
      } yield 1) sum

      val sundaysBeforeDate1 = (for {
        m <- 1 to m1.toInt
        if wday(y1, m, 1) == 0
      } yield 1) sum

      val sundaysAfterDate2 = (for {
        m <- m2.toInt + 1 to 12
        if wday(y2, m, 1) == 0
      } yield 1) sum

      val num = if (d1 == 1 && wday(y1, m1.toInt, 1) == 0)
        sundays + 1 - sundaysBeforeDate1 - sundaysAfterDate2
      else
        sundays - sundaysBeforeDate1 - sundaysAfterDate2
      println(num)
    }
  }
}
