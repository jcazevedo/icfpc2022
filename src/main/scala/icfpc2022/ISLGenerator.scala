package icfpc2022

import icfpc2022.syntax._

trait ISLGenerator[A] {
  def isl(x: A): String
}

object ISLGenerator {
  implicit val lineCutMoveISL: ISLGenerator[LineCutMove] =
    (x: LineCutMove) => s"cut [${x.blockId}] ${x.orientation.isl} ${x.offset}"

  implicit val verticalISL: ISLGenerator[LineCutMove.Vertical.type] =
    (_: LineCutMove.Vertical.type) => "x"

  implicit val horizontalISL: ISLGenerator[LineCutMove.Horizontal.type] =
    (_: LineCutMove.Horizontal.type) => "y"

  implicit val orientationISL: ISLGenerator[LineCutMove.Orientation] = {
    case LineCutMove.Vertical   => LineCutMove.Vertical.isl
    case LineCutMove.Horizontal => LineCutMove.Horizontal.isl
  }

  implicit val pointCutMoveISL: ISLGenerator[PointCutMove] =
    (x: PointCutMove) => s"cut [${x.blockId}] ${x.offset.isl}"

  implicit val coordsISL: ISLGenerator[Coords] =
    (x: Coords) => s"[${x.x}, ${x.y}]"

  implicit val colorMoveISL: ISLGenerator[ColorMove] =
    (x: ColorMove) => s"color [${x.blockId}] ${x.color.isl}"

  implicit val colorISL: ISLGenerator[Color] =
    (x: Color) => s"[${x.r}, ${x.g}, ${x.b}, ${x.a}]"

  implicit val swapMoveISL: ISLGenerator[SwapMove] =
    (x: SwapMove) => s"swap [${x.blockId1}] [${x.blockId2}]"

  implicit val mergeMoveISL: ISLGenerator[MergeMove] =
    (x: MergeMove) => s"merge [${x.blockId1}] [${x.blockId2}]"

  implicit val moveISL: ISLGenerator[Move] = {
    case x: LineCutMove  => x.isl
    case x: PointCutMove => x.isl
    case x: ColorMove    => x.isl
    case x: SwapMove     => x.isl
    case x: MergeMove    => x.isl
  }

  implicit val programISL: ISLGenerator[Program] =
    (x: Program) => x.moves.reverse.map(_.isl).mkString("\n")
}
