package challenge

case class Coordinate(x: Int, y:Int) {
  def up    = this.copy(y = this.y - 1)
  def down  = this.copy(y = this.y + 1)
  def left  = this.copy(x = this.x - 1)
  def right = this.copy(x = this.x + 1)
  def valid(maxX: Int, maxY: Int): Boolean = this.x >= 0 && this.y >= 0 && this.x < maxX && this.y < maxY
}

object Challenge extends App {

  val map1 = Array(
    Array("@", "-", "-", "-", "A", "-", "-", "-", "+"),
    Array("" , "" , "" , "" , "" , "" , "" , "" , "|"),
    Array("x", "-", "B", "-", "+", "" , "" , "" , "C"),
    Array("" , "" , "" , "" , "|", "" , "" , "" , "|"),
    Array("" , "" , "" , "" , "+", "-", "-", "-", "+"),
  )

  val map2 = Array(
    Array("@", "" , "" , "" , "" , "" , "" , "" , "" , "" ),
    Array("|", "" , "C", "-", "-", "-", "-", "+", "" , "" ),
    Array("A", "" , "|", "" , "" , "" , "" , "|", "" , "" ),
    Array("+", "-", "-", "-", "B", "-", "-", "+", "" , "" ),
    Array("" , "" , "|", "" , "" , "" , "" , "" , "" , "x"),
    Array("" , "" , "|", "" , "" , "" , "" , "" , "" , "|"),
    Array("" , "" , "+", "-", "-", "-", "D", "-", "-", "+"),
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

    val maxY = map.length
    val maxX = map.head.length

    while (current._1 != "x") {
      val possibleNeighbours = current._1 match  {
        case "@" => List(current._2.up, current._2.down, current._2.left, current._2.right)
        case "-" => if(previous._1 == "|") List(current._2.up, current._2.down) else List(current._2.left, current._2.right)
        case "|" => List(current._2.up, current._2.down)
        case "+" => {
            if(previous._1 == "-") List(current._2.up, current._2.down)
            else if (previous._1 == "|") List(current._2.left, current._2.right)
            else if (previous._2.y == current._2.y + 1) List(current._2.left, current._2.right)
            else if (previous._2.y == current._2.y - 1) List(current._2.left, current._2.right)
            else if (previous._2.x == current._2.x + 1) List(current._2.up, current._2.down)
            else if (previous._2.x == current._2.x - 1) List(current._2.up, current._2.down)
            else Nil //todo
        }
        case normal: String => {
            List(current._2.up, current._2.down, current._2.left, current._2.right)
        }
      }
      val next = possibleNeighbours
        .filter(_.valid(maxX, maxY))
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
  println(solve(map2).reverse.map(_._1))

}