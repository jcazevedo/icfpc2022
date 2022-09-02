package icfpc2022

case class Canvas(shape: Shape, blocks: Map[String, Block])

object Canvas {
  def blank(width: Int, height: Int): Canvas = {
    val fullShape = Shape(Coords(0, 0), Coords(width - 1, height - 1))
    Canvas(fullShape, Map("0" -> SimpleBlock(fullShape, Color(255, 255, 255, 255))))
  }
}
