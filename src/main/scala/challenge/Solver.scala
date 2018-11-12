package challenge

object Solver {

  private val pathCharacters: Set[Char] = Set('-', '|')

  private final case class Entry(value: Char, pos: Coordinate)

  private def direction(c: Coordinate, p: Coordinate): String = {
    if (c.x == p.x + 1 && c.y == p.y) return "right"
    if (c.x == p.x - 1 && c.y == p.y) return "left"
    if (c.x == p.x  && c.y == p.y + 1) return "down"
    if (c.x == p.x  && c.y == p.y - 1) return "up"
    throw new Exception("")
  }

  private def initial(map: AsciiMap): Either[String, Entry] = {
    val initialPositions =
      map.allPositions.map(c => Entry(map.get(c), c)).filter(_.value == '@')
    if (initialPositions.length == 1)
      Right(initialPositions.head)
    else
      Left("There is no unique initial position.")
  }

  def solve(map: AsciiMap)(initial: Entry): Either[String, (String, String)] = {

    var current = initial
    var previous: Option[Entry] = None

    var path = List[Entry]()
    var letters = List[Char]()

    while (current.value != 'x') {

      val visited: Set[Coordinate] = path.map(_.pos).toSet

      val possibleNeighbours: List[Coordinate] =
        previous match {
          case Some(p) => {
            (current.value match {
              case chr: Char if pathCharacters(chr) => map.next(current.pos)(direction(current.pos, p.pos)).toList
              case _ => (map.next(current.pos)(direction(current.pos, p.pos)).toList ++ map.allNeighbours(current.pos)).filter(!visited(_))
            }).filter(_ != p.pos)
          }
          case None => map.allNeighbours(current.pos)
        }

      if (possibleNeighbours.isEmpty) return Left("No available tiles")
      val next = possibleNeighbours.head

      val entry = Entry(map.get(next), next)
      path = current :: path
      if (!visited(current.pos) && Character.isLetter(current.value)) {
        letters = current.value :: letters
      }
      previous = Some(current)
      current = entry
    }

    path = current :: path
    Right(letters.reverse.mkString, path.reverse.map(_.value).mkString)
  }

  def apply(value: String): Either[String, (String, String)] = AsciiMap(value).flatMap( map => initial(map).flatMap(solve(map)))

}