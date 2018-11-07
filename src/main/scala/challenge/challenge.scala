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
    var previous: Entry = Entry('Š', Coordinate(-1, -1))//todo

    var path = List[Entry]()
    var letters = List[Char]()

    while (current.value != 'x') {
      val visited: Set[Coordinate] = path.map(_.pos).toSet
      val possibleNeighbours = current.value match  {
        case '@' => map.allNeighbours(current.pos)
        case '-' => if(visited(current.pos)) map.verticalNeighbours(current.pos) else map.horizontalNeighbours(current.pos)
        case '|' => if(visited(current.pos)) map.horizontalNeighbours(current.pos) else map.verticalNeighbours(current.pos)
        case '+' => map.allNeighbours(current.pos)
        case _ => map.allNeighbours(current.pos).filter(!visited(_))
      }
      val next = possibleNeighbours
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