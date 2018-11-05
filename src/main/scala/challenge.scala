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

  val map3 = Array(
    Array("" , "" , "@", "-", "-", "-", "+", "-", "-", "-"),
    Array("" , "" , "" , "" , "" , "" , "B", "" , "" , "" ),
    Array("K", "-", "-", "-", "-", "-", "|", "-", "-", "A"),
    Array("|", "" , "" , "" , "" , "" , "|", "" , "" , "|"),
    Array("|", "" , "" , "+", "-", "-", "E", "" , "" , "|"),
    Array("|", "" , "" , "|", "" , "" , "" , "" , "" , "|"),
    Array("+", "-", "-", "E", "-", "-", "E", "x", "" , "C"),
    Array("" , "" , "" , "|", "" , "" , "" , "" , "" , "|"),
    Array("" , "" , "" , "+", "-", "-", "F", "-", "-", "+"),
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
      val visited: Set[Coordinate] = path.map(_._2).toSet
      val possibleNeighbours = current._1 match  {
        case "@" => List(current._2.up, current._2.down, current._2.left, current._2.right)
        case "-" => if(visited(current._2)) List(current._2.up, current._2.down) else List(current._2.left, current._2.right)
        case "|" => if(visited(current._2)) List(current._2.left, current._2.right) else List(current._2.up, current._2.down)
        case "+" => List(current._2.up, current._2.down, current._2.left, current._2.right)
        case normal: String => List(current._2.up, current._2.down, current._2.left, current._2.right).filter(!visited(_))
      }
      val next = possibleNeighbours
        .filter(a => a.valid(maxX, maxY))
        .filter(_ != previous._2)
        .find(n => map(n.y)(n.x) != "")
        .get //todo check for multiple
      val entry = (map(next.y)(next.x), next)
      path = current :: path
      previous = current
      current = entry
    }

    path = current :: path
    path
  }

  val sol1 = solve(map1).reverse.map(_._1).mkString
  require( sol1 == "@---A---+|C|+---+|+-B-x")
  require(solve(map2).reverse.map(_._1).mkString == "@|A+---B--+|+----C|-||+---D--+|x")
  require(solve(map3).reverse.map(_._1).mkString == "@---+B||E--+|E|+--F--+|C|||A--|-----K|||+--E--Ex")

}