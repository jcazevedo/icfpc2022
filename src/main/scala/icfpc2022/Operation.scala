package icfpc2022

sealed trait Operation

sealed trait CutMove extends Operation

case class LineCutMove(blockId: String, orientation: LineCutMove.Orientation, offset: Int) extends CutMove

object LineCutMove {
  sealed trait Orientation
  case object Vertical extends Orientation
  case object Horizontal extends Orientation
}

case class PointCutMove(blockId: String, offset: Coords) extends CutMove

case class ColorMove(blockId: String, color: Int) extends Operation

case class SwapMove(blockId1: String, blockId2: String) extends Operation

case class MergeMove(blockId1: String, blockId2: String) extends Operation
