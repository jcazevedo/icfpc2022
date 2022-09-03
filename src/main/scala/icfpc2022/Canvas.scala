package icfpc2022

case class Canvas(shape: Shape, blocks: Map[String, Block]) {
  lazy val blockSet: Set[Block] =
    blocks.values.toSet

  lazy val coordsSet: Set[Coords] =
    blocks.values.flatMap {
      case SimpleBlock(shape, _)   => Set(shape.bottomLeft)
      case ComplexBlock(_, blocks) => blocks.map(_.shape.bottomLeft).toSet
    }.toSet
}

object Canvas {
  def blank(width: Int, height: Int): Canvas = {
    val fullShape = Shape(Coords(0, 0), width, height)
    Canvas(fullShape, Map("0" -> SimpleBlock(fullShape, Color(255, 255, 255, 255))))
  }
}
