package icfpc2022

import java.awt.image.BufferedImage

object Interpreter {
  def apply(program: Program, move: Move): Either[String, Program] =
    ???

  def paint(program: Program): BufferedImage = {
    val image =
      new BufferedImage(program.canvas.shape.to.x + 1, program.canvas.shape.to.y + 1, BufferedImage.TYPE_INT_ARGB)

    def paintShape(shape: Shape, color: Color): Unit =
      (shape.from.x to shape.to.x).foreach(x =>
        (shape.from.y to shape.to.y).foreach(y => {
          println(s"Setting $x, $y to ${color.toInt.toHexString}"); image.setRGB(x, y, color.toInt)
        })
      )

    def paintBlock(block: Block): Unit =
      block match {
        case SimpleBlock(_, shape, color)    => paintShape(shape, color)
        case ComplexBlock(_, _, childBlocks) => childBlocks.foreach(paintBlock)
      }

    program.canvas.blocks.foreach(paintBlock)

    image
  }
}
