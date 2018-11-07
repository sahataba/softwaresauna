package challenge

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
      map.allPositions.
        map(c => Entry(map.get(c), c)).
        filter(_.value == '@')

    require(initialPositions.length == 1)

    val initial = initialPositions.head
    var current = initial
    var previous: Option[Entry] = None

    var path = List[Entry]()
    var letters = List[Char]()

    def direction(c: Coordinate, p: Coordinate): String = {
      if (c.x == p.x + 1 && c.y == p.y) return "right"
      if (c.x == p.x - 1 && c.y == p.y) return "left"
      if (c.x == p.x  && c.y == p.y + 1) return "down"
      if (c.x == p.x  && c.y == p.y - 1) return "up"
      throw new Exception("")
    }

    while (current.value != 'x') {

      val visited: Set[Coordinate] = path.map(_.pos).toSet

      val possibleNeighbours: List[Coordinate] =
        previous match {
          case Some(p) => {
            current.value match {
              case '+' => map.allNeighbours(current.pos)
              case '-' => map.next(current.pos)(direction(current.pos, p.pos)).toList
              case '|' => map.next(current.pos)(direction(current.pos, p.pos)).toList
              case _ => map.allNeighbours(current.pos).filter(!visited(_))
            }
          }
          case None => map.allNeighbours(current.pos)
        }

      val withoutPrevious = possibleNeighbours.filter(_ != previous.map(_.pos).getOrElse(Entry(' ', Coordinate(-1, -1))))

      val next = withoutPrevious
        .find(n => map.get(n) != ' ')
        .get //todo check for multiple

      val entry = Entry(map.get(next), next)
      path = current :: path
      if (!visited(current.pos) && Character.isLetter(current.value)) {
        letters = current.value :: letters
      }
      previous = Some(current)
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