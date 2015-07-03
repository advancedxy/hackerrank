object Solution {


  // see happy number(https://en.wikipedia.org/wiki/Happy_number)
  // and https://ser.cs.fit.edu/ser2012/problems/division_1/SER2012%20Problem%20Set%20-%20Division%20I.pdf
  // based on https://ser.cs.fit.edu/ser2012/problems/division_1/I_unhappy/
  def calcUnhappyNumbers(n: Int): Int = {
    val p = 1e9.toInt + 7


    // -1 -> Not yet calculate, -2 -> Calculating
    // 0 -> Happy, 1 -> Unhappy
    val unHappy = Array.fill(81 * n + 1 max 1000)(-1)
    unHappy(0) = 0; unHappy(1) = 0 // set 0 and 1 to happy

    def calU(n: Int): Int = {
      if (unHappy(n) >= 0) unHappy(n)
      else if (unHappy(n) == -2) {
        unHappy(n) = 1
        1
      } else {
        unHappy(n) = -2
        var (n_, s_) = (n, 0)
        while (n_ > 0) {
          s_ += (n_ % 10) * (n_ % 10)
          n_ /= 10
        }
        unHappy(n) = calU(s_)
        unHappy(n)
      }
    }

    // DP[PartialSumofDigits][remaingDigits] -> how many unhappy number can we generete

    val DP = Array.fill(81 * n + 1)(Array.fill(n + 1)(-1))

    def calDP(ps: Int, rd: Int): Int = {
      if (DP(ps)(rd) != -1) DP(ps)(rd)
      else if (rd == 0) {
        DP(ps)(0) = calU(ps)
        DP(ps)(rd)
      } else {
        DP(ps)(rd) = 0
        for (i <- 0 to 9)
          DP(ps)(rd) = (DP(ps)(rd) + calDP(ps + i * i, rd - 1)) % p

        DP(ps)(rd)
      }
    }

    calDP(0, n)
  }

  def main(args: Array[String]) {
    val k = readInt
    println(calcUnhappyNumbers(k))
  }
}
