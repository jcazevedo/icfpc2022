package icfpc2022

import java.io.File
import javax.imageio.ImageIO

object Main extends App {
  // val image = ImageIO.read(new File("problems/1.png"))

  // println(image.getHeight())
  // println(image.getWidth())
  // println(image.getRGB(10, 10).toHexString)

  val p = Program(
    Canvas(
      Shape(Coords(0, 0), Coords(100, 200)),
      Map("0" -> SimpleBlock(Shape(Coords(0, 0), Coords(100, 200)), Color(255, 0, 0, 255)))
    )
  )

  val image = Interpreter.paint(p)

  ImageIO.write(image, "png", new File("problems/test.png"))
}
