package icfpc2022

case class Shape(bottomLeft: Coords, width: Int, height: Int) {
  def size: Int =
    width * height

  def topRight: Coords =
    Coords(bottomLeft.x + width, bottomLeft.y + height)

  def within(other: Shape): Boolean =
    this.bottomLeft.x >= other.bottomLeft.x && this.bottomLeft.y >= other.bottomLeft.y && this.topRight.x <= other.topRight.x && this.topRight.y <= other.topRight.y
}

object Shape {
  implicit val shapeOrdering: Ordering[Shape] =
    Ordering.by(shape => (shape.bottomLeft, shape.width, shape.height))
}
