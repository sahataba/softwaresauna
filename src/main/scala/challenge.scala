package challenge

case class Coordinate(x: Int, y:Int) {
  def up    = this.copy(y = this.y - 1)
  def down  = this.copy(y = this.y + 1)
  def left  = this.copy(x = this.x - 1)
  def right = this.copy(x = this.x + 1)
  def valid = this.x >= 0 && this.y >= 0
}

object Challenge extends App {

  val map1 = Array(
    Array("@", "-", "-", "-", "A", "-", "-", "-", "+"),
    Array("" , "" , "" , "" , "" , "" , "" , "" , "|"),
    Array("x", "-", "B", "-", "+", "" , "" , "" , "C"),
    Array("" , "" , "" , "" , "|", "" , "" , "" , "|"),
    Array("" , "" , "" , "" , "+", "-", "-", "-", "+"),
  )


  private def solve(map: Array[Array[String]]): List[(String, Coordinate)] = {
    val initialPositions =
      for {
        (row, y)   <- map.zipWithIndex
        (entry, x) <- row.zipWithIndex
        if entry == "@"
      } yield (entry, Coordinate(x, y))

    require(initialPositions.length == 1)

    var initial = initialPositions.head
    var current = initial
    var previous: (String, Coordinate) = ("", Coordinate(-1, -1))//todo

    var path = List[(String, Coordinate)]()

    while (current._1 != "x") {
      val possibleNeighbours = current._1 match  {
        case "@" => {
            List(current._2.up, current._2.down, current._2.left, current._2.right)
        }
        case "-" => {
            List(current._2.left, current._2.right)
        }
        case "|" => {
            List(current._2.up, current._2.down)
        }
        case "+" => {
            (if(previous._1 == "-") List(current._2.up, current._2.down)
            else if (previous._1 == "|") List(current._2.left, current._2.right)
            else List())
        }
        case normal: String => {
            (if(previous._1 == "-") List(current._2.left, current._2.right)
            else if (previous._1 == "|") List(current._2.up, current._2.down)
            else List())
        }
      }
      val next = possibleNeighbours
        .filter(_.valid)
        .filter(_ != previous._2)
        .find(n => map(n.y)(n.x) != "")
        .get //todo check for multiple
      val entry = (map(next.y)(next.x), next)
      path = entry :: path
      previous = current
      current = entry
    }

    path
  }

  println(solve(map1).reverse.map(_._1))

}