package icfpc2022

case class Shape(from: Coords, to: Coords) {
  def size: Int = (to.x - from.x) * (to.y - from.y)
}
