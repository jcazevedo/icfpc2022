package icfpc2022

sealed trait Move

sealed trait CutMove extends Move

case class LineCutMove(blockId: String, orientation: LineCutMove.Orientation, offset: Int) extends CutMove

object LineCutMove {
  sealed trait Orientation
  case object Vertical extends Orientation
  case object Horizontal extends Orientation
}

case class PointCutMove(blockId: String, offset: Coords) extends CutMove

case class ColorMove(blockId: String, color: Color) extends Move

case class SwapMove(blockId1: String, blockId2: String) extends Move

case class MergeMove(blockId1: String, blockId2: String) extends Move
