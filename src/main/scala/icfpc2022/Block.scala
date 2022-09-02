package icfpc2022

sealed trait Block {
  def blockId: String
  def shape: Shape
}

case class SimpleBlock(blockId: String, shape: Shape, color: Color) extends Block

case class ComplexBlock(blockId: String, shape: Shape, childBlocks: Set[Block]) extends Block
