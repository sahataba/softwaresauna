package challenge

case class Coordinate(x: Int, y:Int) {
  def up    = this.copy(y = this.y - 1)
  def down  = this.copy(y = this.y + 1)
  def left  = this.copy(x = this.x - 1)
  def right = this.copy(x = this.x + 1)
  def valid(maxX: Int, maxY: Int): Boolean = this.x >= 0 && this.y >= 0 && this.x < maxX && this.y < maxY
}

case class Entry(value: Char, pos: Coordinate)

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


  private def solve(map: Array[Array[String]]): (String, String) = {
    val initialPositions =
      for {
        (row, y)   <- map.zipWithIndex
        (entry, x) <- row.zipWithIndex
        if entry == "@"
      } yield Entry(entry(0), Coordinate(x, y))

    require(initialPositions.length == 1)

    val initial = initialPositions.head
    var current = initial
    var previous: Entry = Entry(' ', Coordinate(-1, -1))//todo

    var path = List[Entry]()
    var letters = List[Char]()

    val maxY = map.length
    val maxX = map.head.length

    while (current.value != 'x') {
      val visited: Set[Coordinate] = path.map(_.pos).toSet
      val possibleNeighbours = current.value match  {
        case '@' => List(current.pos.up, current.pos.down, current.pos.left, current.pos.right)
        case '-' => if(visited(current.pos)) List(current.pos.up, current.pos.down) else List(current.pos.left, current.pos.right)
        case '|' => if(visited(current.pos)) List(current.pos.left, current.pos.right) else List(current.pos.up, current.pos.down)
        case '+' => List(current.pos.up, current.pos.down, current.pos.left, current.pos.right)
        case normal => List(current.pos.up, current.pos.down, current.pos.left, current.pos.right).filter(!visited(_))
      }
      val next = possibleNeighbours
        .filter(a => a.valid(maxX, maxY))
        .filter(_ != previous.pos)
        .find(n => map(n.y)(n.x) != "")
        .get //todo check for multiple

      val entry = Entry(map(next.y)(next.x)(0), next)
      path = current :: path
      if (!visited(current.pos) && Character.isLetter(current.value)) {
        letters = current.value :: letters
      }
      previous = current
      current = entry
    }

    path = current :: path
    (letters.reverse.mkString, path.reverse.map(_.value).mkString)
  }

  val correctSolutions = List(
    ("ACB",       "@---A---+|C|+---+|+-B-x"),
    ("ABCD",      "@|A+---B--+|+----C|-||+---D--+|x"),
    ("BEEFCAKE",  "@---+B||E--+|E|+--F--+|C|||A--|-----K|||+--E--Ex")
  )
  val solutions = List(map1, map2, map3).map(solve)
  println(solutions)
  require(solutions == correctSolutions)

}