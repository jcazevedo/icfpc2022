package icfpc2022

import io.circe.{Decoder, Encoder}

case class Coords(x: Int, y: Int)

object Coords {
  implicit val coordsOrdering: Ordering[Coords] =
    Ordering.by(coords => (coords.x, coords.y))

  implicit val coordsEncoder: Encoder[Coords] = Encoder[(Int, Int)].contramap(coords => (coords.x, coords.y))
  implicit val coordsDecoder: Decoder[Coords] = Decoder[(Int, Int)].map({ case (x, y) => Coords(x, y) })
}
