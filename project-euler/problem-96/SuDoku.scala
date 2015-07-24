object Solution {
  type Puzzle = List[List[Int]]
  type Group = List[Int]
  val groups: List[Group] = {
    val rows = for (i <- (0 to 8).toList)
               yield (0 to 80).toList.filter(_ / 9 == i)
    val columns = for (i <- (0 to 8).toList)
                  yield (0 to 80).toList.filter(_ % 9 == i)
    val boxes = for (i <- (0 to 2).toList; j <- 0 to 2)
                yield (0 to 80).toList.filter(n => n / 27 == i && n % 9 / 3 == j)
    rows ::: columns ::: boxes
  }

  def fixedPoint[A](x0: A)(f: (A) => A): A = {
    val x = f(x0)
    if (x == x0) x
    else fixedPoint(x)(f)
  }

  def solve1(puzzle: Puzzle) =
    groups.foldLeft(puzzle) { (puzzle, group) =>
      val usedDigits = group.map(puzzle(_)).filter(_.size == 1).map(_.head).toSet
      puzzle.zipWithIndex.map { case (entry, index) =>
        if (!group.contains(index) || entry.size == 1)
          entry
        else
          entry.filter(!usedDigits(_)) }
    }

  def solve2(puzzle: Puzzle) =
    groups.foldLeft(puzzle) { (puzzle, group) =>
      val v =
        (1 to 9).filter(d => group.filter(puzzle(_).size > 1)
                          .flatMap(puzzle(_))
                          .count(_ == d) == 1)

      v.foldLeft(puzzle) { (puzzle, d) =>
        val start =
          puzzle.zipWithIndex.map { case (entry, index) =>
            if (group.contains(index) && entry.contains(d))
              List(d)
            else
              entry
          }
        fixedPoint(start)(solve1)
      }
    }

  def solve1and2(puzzle: Puzzle) =
    fixedPoint(fixedPoint(puzzle)(solve1))(solve2)

  def isValid(puzzle: Puzzle) =
    groups.forall(g => g.flatMap(puzzle(_)).toSet.size == 9)

  def solve3(puzzle: Puzzle): Option[Puzzle] =
    if (!isValid(puzzle))
      None
    else
      puzzle.indexWhere(_.size > 1) match {
        case -1 =>
          Some(puzzle)
        case index =>
          puzzle(index).flatMap { guess =>
            solve3(fixedPoint(puzzle.updated(index, List(guess)))(solve1and2))
          }.headOption
      }

  def main(args: Array[String]) {
    val puzzle =
      (1 to 9).toList.flatMap(x =>
        readLine.map(char => {
                       char match {
                         case '0' => (1 to 9).toList
                         case c => List(c.asDigit)
                       }
                     }
        )
      )

    val solvedPuzzle = solve3(puzzle).get
    print(solvedPuzzle.map(_.head).grouped(9).map(x => x.mkString).mkString("\n"))
  }
}
