package icfpc2022

sealed trait Block {
  def blockId: String
}

case class SimpleBlock(blockId: String, shape: Shape, color: Int) extends Block

case class ComplexBlock(blockId: String, shape: Shape, childBlocks: Set[Block]) extends Block
