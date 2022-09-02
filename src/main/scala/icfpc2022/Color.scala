package icfpc2022

case class Color(r: Int, g: Int, b: Int, a: Int) {
  def toInt: Int =
    (a << 24) + (r << 16) + (g << 8) + b
}
