object Solution {
  import scala.collection.mutable.HashMap

  def markovChainSolver(dice: Int,
                        k: Int,
                        iterator:Int = 150): Vector[String] = {

    val names = Vector("GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2",
                       "B3", "JAIL", "C1", "U1", "C2", "C3", "R2", "D1", "CC2",
                       "D2", "D3", "FP", "E1", "CH2", "E2", "E3", "R3", "F1",
                       "F2", "U2", "F3", "G2J", "G1", "G2", "CC3", "G3", "R4",
                       "CH3", "H1", "T2", "H2")

    val squares: Map[String, Int] = names.zipWithIndex.toMap

    def nextHelper(acc: Map[String, Int],
                   indices: List[Int],
                   start: Int): Map[String, Int] = indices match {
      case h :: t =>
        val newAcc =
          if (start <= h)
            (start until h).map(names(_) -> h).toMap ++ acc 
          else{
            assert(start < names.size)

            (start until h + names.size)
              .map(x => names(x % names.size) -> h).toMap ++ acc
          }
        nextHelper(newAcc, t, h)
      case Nil => acc
    }
    
    val nextRSquares: Map[String, Int] = {
      val rSquaresIndex =
        squares.filter(x => x._1.startsWith("R")).map(_._2).toList.sorted

      nextHelper(Map.empty[String, Int], rSquaresIndex, rSquaresIndex.last)
    }

    val nextUSquares: Map[String, Int] = {
      val uSquareIndex =
        squares.filter(x => x._1.startsWith("U")).map(_._2).toList.sorted

      nextHelper(Map.empty[String, Int], uSquareIndex, uSquareIndex.last)
    }


    def limit(squareNumber: Int): Int =
        (squareNumber + names.size) % names.size

    def nextSquare(square: Int, roll: Int): Vector[Double] = {

      val newSquare: Int = (square + roll) % names.size
      val nexts: Array[Double] = {
        val result = Array.fill(names.size)(0D)
        val p = 0.0625D // 1 / 16
        names(newSquare) match {
          case "G2J" =>
            result(squares("JAIL")) = 1D
          case "CC1" | "CC2" | "CC3" =>
            result(squares("JAIL")) = p
            result(squares("GO")) = p
            result(newSquare) =  p * 14
        // CH3 go back 3 quares will be CC3, need to be dealed differently.
        case "CH1" | "CH2" =>
          val squareList =
            List(squares("GO"), squares("JAIL"), squares("C1"), squares("E3"),
                 squares("H2"), squares("R1"), nextRSquares(names(newSquare)),
                 nextRSquares(names(newSquare)), nextUSquares(names(newSquare)),
                 limit(newSquare - 3))
          for (index <- squareList) {
            result(index) += p
          } 
          result(newSquare) += 6 * p
        case "CH3" =>
          val squareList =
            List(squares("GO"), squares("JAIL"), squares("C1"), squares("E3"),
                 squares("H2"), squares("R1"), nextRSquares(names(newSquare)),
                 nextRSquares(names(newSquare)), nextUSquares(names(newSquare))
                 )
          for (index <- squareList) {
            result(index) += p 
          } 
          result(newSquare) +=  p * 6 
          result(squares("GO")) += p * p
          result(squares("JAIL")) += p * p
          result(limit(newSquare - 3)) += p * 14 * p
        case _ =>
          result(newSquare) = 1D
        }
        result
      }
      names.indices.toVector.map( next =>
        nexts(next) / (dice * dice)
      )
    }

    // P is the Markov matrix
    val P: Vector[Vector[Double]] = {
      val zeroVector = Vector.fill(names.size)(0D)
      val rolls = for {
        dice1 <- 1 to dice
        dice2 <- 1 to dice
      } yield dice1 + dice2

      def row(i: Int): Vector[Double] =
        rolls.toList.foldLeft(zeroVector) { (vec, roll) =>
        (vec, nextSquare(i, roll)).zipped.map(_ + _)
      }
      names.indices.toVector.map(row(_))
    }

    def matrixMul(m1: Vector[Vector[Double]],
                  m2: Vector[Vector[Double]]) = {
      val size = m1.size
      val result = Array.fill(size)(Array.fill(size)(0D))

      for {
        i <- 0 until size
        j <- 0 until size
        k <- 0 until size
      } result(i)(j) += m1(i)(k) * m2(k)(j)

      result.map(_.toVector).toVector
    }

    // as k approaches infinity, all rows of P^k approach the stationary probability vector.
    // trial and error says k=150 gives accuracy to at least 3 decimal places
    val stationaryProbabilityVector =
      Iterator.iterate(P)(matrixMul(_, P)).drop(iterator).next.head.map(_.toDouble)

    val probabs = (stationaryProbabilityVector
                     .zipWithIndex.sortBy(-_._1).map(x => names(x._2) -> x._1))
    // println(probabs, probabs.map(_._2).sum)
    stationaryProbabilityVector
      .zipWithIndex.sortBy(-_._1).take(k).map(x => names(x._2))
  }

  def main(args: Array[String]) {
    val Array(n, k) = readLine.split(" ").map(_.toInt)
    // 50 iteration is enough to get the precision we want.
    val topNames = markovChainSolver(n, k, 50)
    println(topNames.mkString(" "))
  }

}
