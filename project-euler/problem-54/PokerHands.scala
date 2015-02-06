object Solution {
  import scala.collection.mutable.HashMap

  case class Card(rank: Int, suit: Char) {}

  class Hand(cards: Array[Card]) {
    require(cards.size == 5)

    // steal from scala stand lib
    def occCount[A](x: Seq[A]) = {
      val occ = new HashMap[A, Int] { override def default(k: A) = 0 }
      for (e <- x) occ(e) += 1
      occ
    }

    val ranks = cards.map(_.rank)
    val suits = cards.map(_.suit)
    
    private val rankOcc = occCount(ranks)
    private val suitOcc = occCount(suits)
    val sortedRanks = ranks.sorted
    val valuesList = sortedRanks.toList.reverse
    val isFlush = suitOcc.exists(_._2 == 5)
    val straight: Option[Int] = {
      if (sortedRanks.head == 2 && sortedRanks.last == 14) { // 2,3,4,5,A
        val lastRanks = sortedRanks.dropRight(1)
        if (lastRanks.zip(lastRanks.tail).forall(x => x._2 - x._1 == 1))
          Some(sortedRanks(sortedRanks.size - 2))
        else None
      }
      else {
        if (sortedRanks.zip(sortedRanks.tail).forall(x => x._2 - x._1 == 1))
          Some(sortedRanks.last)
        else
          None
      }
    }
    
    val score = rankOcc.toList.map(x => (x._2, x._1)).sorted.reverse match {
      case _ if(isFlush && straight.isDefined) => 9 :: straight.get :: Nil
      case List((4, a), (1, b)) => List(8, a, b)
      case List((3, a), (2, b)) => List(7, a, b)
      case _ if (isFlush) => 6 :: valuesList
      case _ if (straight.isDefined) => 5 :: straight.get :: Nil
      case List((3, a), (1, b), (1, c)) => List(4, a, b, c)
      case List((2, a), (2, b), (1, c)) => List(3, a, b, c)
      case List((2, a), (1, b), (1, c), (1, d)) => List(2, a, b, c, d)
      case _ => 1 :: valuesList
    }
    
    def compareTo(that: Hand): Int = 
      score.zip(that.score).dropWhile(p => p._1 == p._2) match {
        case (a, b) :: _ => if (a > b) 1 else -1
        case Nil => 0
      }
  }
  def parseCards(cs: Array[String]): Array[Card] = {
    def charToRank(c: Char): Int = c match {
      case 'T' => 10
      case 'J' => 11
      case 'Q' => 12
      case 'K' => 13
      case 'A' => 14
      case _   => c.asDigit
    }

    cs.map(s => Card(charToRank(s(0)), s(1)))
  }

  def main(args: Array[String]) {
    val t = readLine.toInt
    for (i <- 1 to t) {
      val cards = readLine.split(" ")
      val (p1, p2) =  cards.splitAt(5)
      val h1 = new Hand(parseCards(p1))
      val h2 = new Hand(parseCards(p2))
      h1.compareTo(h2) match {
        case 1 => println("Player 1")
        case -1 => println("Player 2")
        case _ =>
      }
    }
  }
}
