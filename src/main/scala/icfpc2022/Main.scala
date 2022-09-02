package icfpc2022

import java.io.File
import javax.imageio.ImageIO

import icfpc2022.syntax._

object Main extends App {
  // val image = ImageIO.read(new File("problems/1.png"))

  // println(image.getHeight())
  // println(image.getWidth())
  // println(image.getRGB(10, 10).toHexString)

  val p = Interpreter.apply(
    Program(Canvas.blank(400, 400)),
    List(
      PointCutMove("0", Coords(100, 100)),
      ColorMove("0.0", Color(255, 0, 0, 255)),
      ColorMove("0.1", Color(0, 255, 0, 255)),
      ColorMove("0.2", Color(0, 0, 255, 255)),
      ColorMove("0.3", Color(0, 0, 0, 255)),
      LineCutMove("0.2", LineCutMove.Vertical, 250),
      ColorMove("0.2.1", Color(255, 0, 0, 255)),
      SwapMove("0.2.1", "0.2.0"),
      MergeMove("0.2.1", "0.2.0"),
      MergeMove("1", "0.3"),
      ColorMove("2", Color(255, 255, 255, 255))
    )
  )

  println(p.toOption.get.isl)

  val image = Interpreter.paint(p.toOption.get)

  ImageIO.write(image, "png", new File("test.png"))
}
