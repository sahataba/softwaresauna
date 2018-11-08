package challenge

object Solver {

  final case class Entry(value: Char, pos: Coordinate)

  def apply(map: AsciiMap): (String, String) = {

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
            (current.value match {
              case '-' => map.next(current.pos)(direction(current.pos, p.pos)).toList
              case '|' => map.next(current.pos)(direction(current.pos, p.pos)).toList
              case _ => map.allNeighbours(current.pos).filter(!visited(_))
            }).filter(_ != p.pos)
          }
          case None => map.allNeighbours(current.pos)
        }

      val next = possibleNeighbours.head //todo check for multiple

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

}