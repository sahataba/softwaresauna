package challenge
import scala.util.Try

final case class AsciiMap(private val map: Array[Array[Char]]) {

  val maxY = map.length
  require(maxY > 0)
  val maxX = map(0).length
  require(maxX > 0)
  map.foreach(l => require(maxX == l.length, "different row length"))

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
    List(coor.up, coor.down, coor.left, coor.right).filter(valid)

  private def valid(coor: Coordinate): Boolean =
    coor.x >= 0 && coor.y >= 0 && coor.x < maxX && coor.y < maxY && get(coor) != ' '
}

object AsciiMap {

  private def toMatrix(value: String): Either[String, Array[Array[Char]]] =
    Try{value.stripMargin.split("\n").map(_.toCharArray)}.toEither.left.map(_.toString)

  private def create(map: Array[Array[Char]]): Either[String, AsciiMap] = Try{AsciiMap(map)}.toEither.left.map(_.toString)

  def apply(value: String): Either[String, AsciiMap] = toMatrix(value).flatMap(create)

}