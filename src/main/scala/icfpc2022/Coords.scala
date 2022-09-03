package icfpc2022

import io.circe.{Decoder, Encoder}

case class Coords(x: Int, y: Int)

object Coords {
  implicit val coordsOrdering: Ordering[Coords] =
    new Ordering[Coords] {
      def compare(x: Coords, y: Coords): Int =
        if (x.x < y.x) -1
        else if (x.x > y.x) 1
        else if (x.y < y.y) -1
        else if (x.y > y.y) 1
        else 0
    }

  implicit val coordsEncoder: Encoder[Coords] = Encoder[(Int, Int)].contramap(coords => (coords.x, coords.y))
  implicit val coordsDecoder: Decoder[Coords] = Decoder[(Int, Int)].map({ case (x, y) => Coords(x, y) })
}
