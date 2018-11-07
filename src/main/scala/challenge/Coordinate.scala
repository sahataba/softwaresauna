package challenge

final case class Coordinate(x: Int, y:Int) {
  lazy val up    = this.copy(y = this.y - 1)
  lazy val down  = this.copy(y = this.y + 1)
  lazy val left  = this.copy(x = this.x - 1)
  lazy val right = this.copy(x = this.x + 1)
}