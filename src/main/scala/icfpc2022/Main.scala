package icfpc2022

import java.io.File
import javax.imageio.ImageIO

object Main extends App {
  val image = ImageIO.read(new File("problems/1.png"))

  println(image.getHeight())
  println(image.getWidth())
  println(image.getRGB(10, 10).toHexString)
}
