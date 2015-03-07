object Solution {
  import scala.collection.mutable.{HashMap, HashSet}

  case class Node(val id: Int) {
    var size = 1
    val children: HashSet[Node] = new HashSet()
  }

  def preProcessNode(root: Node) {
    if (root.children.isEmpty) return
    else for (cNode <- root.children) {
      preProcessNode(cNode)
      root.size += cNode.size
    }
  }

  def countCuts(node: Node): Int = {
    var count = 0
    for (cNode <- node.children) {
      if (cNode.size % 2 == 0) count += 1
      count += countCuts(cNode)
    }
    count
  }

  def main(args: Array[String]) {
    val nodeMap = new HashMap[Int, Node]() {
      override def default(x: Int) = {
        val nNode = Node(x)
        this(x) = nNode
        nNode
      }
    }

    val Array(n, m) = readLine.split(" ").map(_.toInt)
    for (_ <- 1 to m) {
      val Array(n1, n2) = readLine.split(" ").map(_.toInt)
      val parent = n1 min n2
      val child = n1 - parent + n2
      val pNode = nodeMap(parent)
      val cNode = nodeMap(child)
      pNode.children.add(cNode)
    }
    val root = nodeMap.keys.min
    preProcessNode(nodeMap(root))
    println(countCuts(nodeMap(root)))
  }
}
