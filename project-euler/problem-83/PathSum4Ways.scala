object Solution {
  import scala.collection.mutable.{HashSet, HashMap, PriorityQueue}

  // Dijkstra's Algorithm
  def minPathSum4(matrix: Array[Array[Int]]): Long = {
    val n = matrix.size
    val target = (n - 1, n - 1)
    val nodes = for {
                   i <- 0 until n
                   j <- 0 until n
                 } yield (i, j)
    // val reverseOrdering = Ordering[(Long, (Int, Int))]
    val nodesMap = Array.fill(n * n)(Long.MaxValue)
    val nodesSet = HashSet(nodes: _*)
    val nodePQueue = PriorityQueue(nodes.map(x => (-Long.MaxValue, x)): _*)
    // (Long.MaxValue, (0, 0)) happens to be the first element in the queue
    val minPath = 0L + matrix(0)(0)
    nodesMap(0) = minPath
    nodePQueue.enqueue((-minPath, (0, 0)))

    def extractMin(): (Long, (Int, Int)) = {
      var a = nodePQueue.dequeue
      while (! nodesSet(a._2)) {
        a = nodePQueue.dequeue
      }
      (-a._1, a._2)
    }

    while (nodesSet.size > 0) {
      val (length, (x, y)) = extractMin()
      // get the neighbors
      for {
        (i, j) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
        if x + i >= 0 && x + i < n &&
        y + j >= 0 && y + j < n && nodesSet((x + i, y + j))
        dis = length + matrix(x + i)(y + j)
        if dis < nodesMap((x + i) * n + y + j)
      } {
        nodesMap((x + i) * n + y + j) = dis
        nodePQueue.enqueue((-dis, (x + i, y + j)))
      }
      nodesSet.remove((x, y))
      nodesMap(x * n + y) = length
      if (x == n - 1 && y == n - 1)
        return length
    }

    nodesMap(n * n - 1)
  }

  def main(args: Array[String]) {
    val n = readInt
    val matrix = (1 to n).map(x => readLine.split(" ").map(_.toInt)).toArray
    println(minPathSum4(matrix))
  }
}
