package challenge

final case class Coordinate(x: Int, y:Int) {
  lazy val up    = this.copy(y = this.y - 1)
  lazy val down  = this.copy(y = this.y + 1)
  lazy val left  = this.copy(x = this.x - 1)
  lazy val right = this.copy(x = this.x + 1)
  lazy val verticalNeighbours = List(this.up, this.down)
  lazy val horizontalNeighbours = List(this.left, this.right)
  lazy val allNeighbours = verticalNeighbours ++ horizontalNeighbours
  def valid(maxX: Int, maxY: Int): Boolean = this.x >= 0 && this.y >= 0 && this.x < maxX && this.y < maxY
}

final case class AsciiMap(map: Array[Array[Char]]) {

  val maxY = map.length
  require(maxY > 0)
  val maxX = map(0).length
  require(maxX > 0)
  map.foreach(l => require(maxX == l.length))

  def get(coor: Coordinate): Char = map(coor.y)(coor.x)
}

final case class Entry(value: Char, pos: Coordinate)

object Challenge extends App {

  val map1 =
    """|@---A---+
       |        |
       |x-B-+   C
       |    |   |
       |    +---+""".stripMargin.split("\n").map(_.toCharArray)

  val map2 =
    """|@         
       || C----+  
       |A |    |  
       |+---B--+  
       |  |      x
       |  |      |
       |  +---D--+""".stripMargin.split("\n").map(_.toCharArray)

  val map3 = 
   """|  @---+   
      |      B   
      |K-----|--A
      ||     |  |
      ||  +--E  |
      ||  |     |
      |+--E--Ex C
      |   |     |
      |   +--F--+""".stripMargin.split("\n").map(_.toCharArray)

  private def solve(map: AsciiMap): (String, String) = {

    val initialPositions =
      for {
        (row, y)   <- map.map.zipWithIndex
        (entry, x) <- row.zipWithIndex
        if entry == '@'
      } yield Entry(entry, Coordinate(x, y))

    require(initialPositions.length == 1)

    val initial = initialPositions.head
    var current = initial
    var previous: Entry = Entry('Š', Coordinate(-1, -1))//todo

    var path = List[Entry]()
    var letters = List[Char]()

    while (current.value != 'x') {
      val visited: Set[Coordinate] = path.map(_.pos).toSet
      val possibleNeighbours = current.value match  {
        case '@' => current.pos.allNeighbours
        case '-' => if(visited(current.pos)) current.pos.verticalNeighbours else current.pos.horizontalNeighbours
        case '|' => if(visited(current.pos)) current.pos.horizontalNeighbours else current.pos.verticalNeighbours
        case '+' => current.pos.allNeighbours
        case normal => current.pos.allNeighbours.filter(!visited(_))
      }
      val next = possibleNeighbours
        .filter(a => a.valid(map.maxX, map.maxY))
        .filter(_ != previous.pos)
        .find(n => map.get(n) != ' ')
        .get //todo check for multiple

      val entry = Entry(map.get(next), next)
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
  val solutions = List(map1, map2, map3).map(m => solve(AsciiMap(m)))
  println(solutions)
  require(solutions == correctSolutions)

}