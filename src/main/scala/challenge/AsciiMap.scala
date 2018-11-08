package challenge

final case class AsciiMap(private val map: Array[Array[Char]]) {

  val maxY = map.length
  require(maxY > 0)
  val maxX = map(0).length
  require(maxX > 0)
  map.foreach(l => require(maxX == l.length))

  def get(coor: Coordinate): Char = map(coor.y)(coor.x)

  def next(coor: Coordinate)(direction: String): Option[Coordinate] = (direction match {
    case "up" => Some(coor.up)
    case "down" => Some(coor.down)
    case "left" => Some(coor.left)
    case "right" => Some(coor.right)
  }).filter(valid)

  val allPositions: List[Coordinate] =
    (for (
      y <- 0 to maxY - 1;
      x <- 0 to maxX - 1) yield Coordinate(x, y)).toList

  def allNeighbours(coor: Coordinate): List[Coordinate] =
    List(coor.up, coor.down, coor.left, coor.right).//todo: bug when changing order
      filter(valid).
      filter(n => get(n) != ' ')

  private def valid(coor: Coordinate): Boolean =
    coor.x >= 0 && coor.y >= 0 && coor.x < maxX && coor.y < maxY
}