package icfpc2022

import java.awt.image.BufferedImage

object Interpreter {
  def apply(program: Program, move: Move): Either[String, Program] =
    ???

  def apply(program: Program, moves: List[Move]): Either[String, Program] = {
    var current: Either[String, Program] = Right(program)
    val vecMoves = moves.toVector
    var i = 0
    while (i < vecMoves.length && current.isRight) {
      current = apply(current.toOption.get, moves(i))
      i += 1
    }
    current
  }

  def paint(program: Program): BufferedImage = {
    val image =
      new BufferedImage(program.canvas.shape.to.x + 1, program.canvas.shape.to.y + 1, BufferedImage.TYPE_INT_ARGB)

    def paintShape(shape: Shape, color: Color): Unit =
      (shape.from.x to shape.to.x).foreach(x =>
        (shape.from.y to shape.to.y).foreach(y => image.setRGB(x, y, color.toInt))
      )

    def paintBlock(block: Block): Unit =
      block match {
        case SimpleBlock(shape, color)    => paintShape(shape, color)
        case ComplexBlock(_, childBlocks) => childBlocks.foreach(paintBlock)
      }

    program.canvas.blocks.values.foreach(paintBlock)

    image
  }
}
