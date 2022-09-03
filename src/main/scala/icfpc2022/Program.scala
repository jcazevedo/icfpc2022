package icfpc2022

case class Program(canvas: Canvas, moves: List[Move], blockId: Long, cost: Long)

object Program {
  def fromInitialCanvas(canvas: Canvas): Program =
    Program(canvas, List.empty, canvas.blocks.keySet.map(_.toLong).max, 0)
}
