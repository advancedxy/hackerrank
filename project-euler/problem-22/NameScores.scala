object Solution {
  
  def scoreOfName(name: String, names: Array[String]): Int = {
    import scala.collection.Searching._
    val index = names.search(name)
    index match {
      case Found(x) => name.map(_ - 'a' + 1).sum * (x + 1)
      case _ => 0
    }
  }

  def main(args: Array[String]) {
    val n = readLine.toInt
    val names = Array.fill(n) {readLine.toLowerCase} sorted
    val q = readLine.toInt
    for (i <- 1 to q) {
      val name = readLine.toLowerCase
      println(scoreOfName(name, names))
    }
  }
}
