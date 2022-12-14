package icfpc2022

import io.circe.{Decoder, Encoder}

case class Color(r: Int, g: Int, b: Int, a: Int) {
  def toInt: Int =
    (a << 24) + (r << 16) + (g << 8) + b
}

object Color {
  def fromInt(x: Int): Color =
    Color(r = (x >> 16) & 0xff, g = (x >> 8) & 0xff, b = x & 0xff, a = (x >> 24) & 0xff)

  implicit val colorEncoder: Encoder[Color] =
    Encoder[(Int, Int, Int, Int)].contramap(color => (color.r, color.g, color.b, color.a))
  implicit val colorDecoder: Decoder[Color] = Decoder[(Int, Int, Int, Int)].map({ case (r, g, b, a) =>
    Color(r, g, b, a)
  })
}
