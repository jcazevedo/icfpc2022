package icfpc2022

import java.awt.image.BufferedImage

object Scorer {
  private def pixelDiff(c1: Color, c2: Color): Double = {
    val r2 = (c1.r - c2.r).toDouble * (c1.r - c2.r).toDouble
    val g2 = (c1.g - c2.g).toDouble * (c1.g - c2.g).toDouble
    val b2 = (c1.b - c2.b).toDouble * (c1.b - c2.b).toDouble
    val a2 = (c1.a - c2.a).toDouble * (c1.a - c2.a).toDouble
    return math.sqrt(r2 + g2 + b2 + a2)
  }

  private def similarity(p: Program, image: BufferedImage): Long = {
    lazy val height = image.getHeight()

    def blockDiff(block: Block): Double = {
      block match {
        case SimpleBlock(shape, color) =>
          (shape.bottomLeft.x until shape.topRight.x)
            .map(x =>
              (shape.bottomLeft.y until shape.topRight.y)
                .map(y => pixelDiff(color, Color.fromInt(image.getRGB(x, height - y - 1))))
                .sum
            )
            .sum

        case ComplexBlock(_, childBlocks) =>
          childBlocks.map(blockDiff).sum
      }
    }

    val diff = p.canvas.blocks.values.map(blockDiff).sum
    val alpha = 0.005

    math.round(diff * alpha)
  }

  def score(program: Program, target: BufferedImage): Long =
    program.cost + similarity(program, target)
}
