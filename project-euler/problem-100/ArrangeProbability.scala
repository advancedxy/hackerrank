import scala.annotation.tailrec

object Solution {
/**
  * This is a Pell like equation problem, see pell.pdf in this dir for details
  *
  * b*(b-1)/(t*(t-1)) = P/Q => P(t^2-t) = Q(b^2-b) =>
  * P(4*t^2-4*t+1) - P = Q(4*b^2-4*b+1) - Q =>
  * P(2*t-1)^2 - Q(2*b-1)^2 = P - Q =>
  * (P(2*t-1))^2 - PQ(2*b-1)^2 = P(P-Q) =>
  * Assume x = P(2*t-1), y = 2*b - 1, D = PQ, k = P(P-Q),
  * x^2 - Dy^2 = k
  */


  def solve_pell_with_limit(p: Long, q: Long, limit: Long): (Long, Long) = {
    val d = p * q
    val n = p * (p - q)
    type PQD = (BigInt, BigInt, Long)
    type Doub = (BigInt, BigInt)


    def PQaStream(p: Long, q: Long, d: Long) = {
      val ATuple: Doub = (BigInt(0), BigInt(1))
      val BTuple: Doub = (BigInt(1), BigInt(0))
      val GTuple: Doub = (BigInt(-p), BigInt(q))

      def nextPQa(pqd: PQD, a: Doub, b: Doub, g: Doub):
          (BigInt, Doub, Doub, Doub, Doub) = {
        val (p, q, d) = pqd
        val ai = ((p + math.sqrt(d).toLong)/q)
        val Ai = ai * a._2 + a._1
        val Bi = ai * b._2 + b._1
        val Gi = ai * g._2 + g._1
        val pNext = ai * q - p
        val qNext = (d - pNext * pNext) / q
        (ai, (a._2, Ai), (b._2, Bi), (g._2, Gi), (pNext, qNext))
      }

      def make(pqd: PQD, a: Doub, b: Doub, g: Doub):
          Stream[(BigInt, Doub, Doub, Doub, Doub)] = {
        val next = nextPQa(pqd, a, b, g)
        val (_, na, nb, ng, (p, q)) = next
        Stream.cons(next, make((p, q, d), na, nb, ng))
      }
      make((BigInt(p), BigInt(q), d), ATuple, BTuple, GTuple)
    }

    // Use brute force to solve the problem
    def findTUForOne(d: Long): (BigInt, BigInt) = {
      val pqaStream = PQaStream(0, 1, d)
      val l = pqaStream.takeWhile(x => x._5._2 != 1).length + 1
      println(l)
      val ll = if (l % 2 == 0) l else 2 * l
      val a = pqaStream.take(ll).last
      (a._4._2, a._3._2)
    }

    // d should not be a perfect square, otherwise p is 1, q is the perfect
    // square, and the only solution will be x=y=1, which is not possible for
    // our requirement.
    val sqrtd = math.sqrt(d).toLong
    if (sqrtd * sqrtd == d) {
      // return (-1, -1) as contracted.
      (-1, -1)
    } else {
      val (t, u) = findTUForOne(d)
      val (l1, l2) =
        if (n > 0) (0D, math.sqrt(n * (t - 1) / (2 * d)))
        else (math.sqrt(-n / d), math.sqrt(-n * (t + 1) / (2 * d)))
      println(l1, l2)
      var fundamentals = List.empty[(Long, Long)]
      var y = math.ceil(l1).toLong
      while (y <= l2.toLong) {
        val x = math.sqrt(n + d * y * y).toLong
        println(x, y)
        if (x * x == n + d * y * y) {fundamentals = (x, y) :: fundamentals}
        y += 1
      }
      println(s"Fundamentals: $fundamentals")

      if (fundamentals.isEmpty) return (-1, -1)
      // For (x, y), (-x, y) is also a solution. Test (-x, y) and (x, y) is in
      // the same class, if not , we should add the (-x, y) to the fundemental
      // solutions and convert it to the mininal positive solution.
      // How can we test (x, y) and (-x, y) is in the same class? According the
      // pell.pdf, (x, y) and (r, s) is in the same class if and only if
      // (xr - Dys)/N and (xs - yr)/N are integers. which means 2*x*y/N should
      // be integer.
      // Because n < 0, looks like we already generate the minimal positive
      // solutions. 
      val fsUnconverted: Seq[(Long, Long)] = fundamentals.flatMap(
        xy => {
          val x = xy._1
          val y = xy._2
          if ((2 * x * y) % n == 0) List((x, y))
          else List((x, y), (-x, y))
        }
      )
      val fs: Seq[(Long, Long)] = fsUnconverted.map(xy =>
        {
          val (x, y) = xy
          if (x < 0) {
            val newX = x * t + y * u * d
            val newY = x * u + y * t
            if (newX > 0) (newX, newY)
            else (-newX, -newY)
          }
          else (x.toLong, y.toLong)
        }
      )
      println(s"Converted fs: $fs")
      val fsFiltered = fs.filter(
        xy => {
          val (x, y) = xy
          val rem = x % p
          if (rem == 0) {
            val div = x / p
            (div & 1) == 1 && div > (2 * limit - 1) && (y & 1) == 1
          } else false
        })
      if (!fsFiltered.isEmpty) return {
        val first = fsFiltered.sorted.head
        ((first._2 + 1) / 2, (first._1 / p + 1) / 2)
      }

      // We get the fundamental solutions. We need to generete the smallest
      // solution > p * (2 * limit - 1) and x should be p * odd number, y should
      // be odd number. But how can we stop searching that we can make sure
      // there is no such solution in this class? There is a limit in the
      // problem statement that t will be less than 2^63, then I think we can
      // test whether there is a solution or not.
      val (t1, u1) = (BigInt(t), BigInt(u))
      var (tc, uc) = (t1, u1)
      var count = 0L
      while (true) {
        val curFS = fs.map(xy =>
          {
            val x = xy._1
            val y = xy._2
            val newX = x * tc + y * uc * d
            val newY = x * uc + y * tc
            (newX, newY)
          }
        )

        if (curFS.forall(xy => xy._1 >= p * (BigInt(2).pow(64) - 1)))
        {
          return (-1, -1)
        }

        println(s"Current FS: $curFS")
        val curFsFiltered = curFS.filter(xy =>
          {
            val x = xy._1
            val y = xy._2
            val rem = x % p
            if (rem == 0) {
              val div = x / p
              (div & 1) == 1 && div > (2 * limit - 1) && (y & 1) == 1
            } else false
          }
        )
        println(s"Filtered FS: $curFsFiltered")
        if (!curFsFiltered.isEmpty) return {
          val first = curFsFiltered.sorted.head
          (((first._2 + 1)/ 2).toLong, ((first._1 / p + 1) / 2).toLong)
        }

        val tcTemp = t * tc + u * uc * d
        val ucTemp = u * tc + t * uc
        tc = tcTemp
        uc = ucTemp
        count += 1
      }
      (-1, -1)
    }
    
    // I intended to solve it by LMM Algorithm mentioned by pell.pdf. But it
    // looks pretty complicated.
    // def factorize(n: Long) = {
    //   var nn = n
    //   val primeMap = new scala.collection.mutable.HashMap[Int, Int] {
    //     override def default(x: Int): Int = 0
    //   }
    //   var i = 0
    //   while (primes(i) <= nn) {
    //     while (nn % primes(i) == 0) {
    //       nn /= primes(i)
    //       primeMap(primes(i)) += 1
    //     }
    //     // Check whether the remain is a prime or not, this can speed up the
    //     // procedure.  
    //     if (nn < primeTable.length && primeTable(nn.toInt)) {
    //       primeMap(nn.toInt) = 1
    //       nn = 1
    //     }
    //     i += 1
    //   }
    //   primeMap
    // }

    // def generateFList(n: Long): Seq[Int] = {
    //   val primeMap = factorize(n)
    //   val preProcessMap = primeMap.mapValues(x => x / 2)
    //   preProcessMap.foldLeft(Seq(1)) { (xList, kv) =>
    //     val (k, v) = kv
    //     val kvList = (0 to v).map(math.pow(k, _).toInt)
    //     for {
    //       x <- xList
    //       y <- kvList
    //     } yield x * y
    //   }
    // }
  }

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def main(args: Array[String]) = {
    val t = readInt
    for (i <- 1 to t) {
      val Array(p, q, limit) = readLine.split(" ").map(_.toLong)
      val g = gcd(p, q)
      val (pp, pq) = (p / g, q / g)
      val (blue, total) = solve_pell_with_limit(pp, pq, limit)
      if (blue < 0) println("No solution") else println(s"$blue $total")
    }
  }
}
