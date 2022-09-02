package icfpc2022

sealed trait Block {
  def shape: Shape
}

case class SimpleBlock(shape: Shape, color: Color) extends Block

case class ComplexBlock(shape: Shape, childBlocks: Set[Block]) extends Block
