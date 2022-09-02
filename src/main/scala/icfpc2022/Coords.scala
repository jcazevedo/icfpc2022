package icfpc2022

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
}
