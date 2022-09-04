package icfpc2022

case class Program(canvas: Canvas, moves: List[Move], blockId: Long, cost: Long, scores: Program.BaseScores)

object Program {
  def fromInitialCanvas(canvas: Canvas, scores: BaseScores): Program =
    Program(canvas, List.empty, canvas.blocks.keySet.map(_.toLong).max, 0, scores)

  case class BaseScores(
      lineCut: Int,
      pointCut: Int,
      color: Int,
      swap: Int,
      merge: Int
  )

  object BaseScores {
    final val default = BaseScores(
      lineCut = 7,
      pointCut = 10,
      color = 5,
      swap = 3,
      merge = 1
    )

    final val v2 = BaseScores(
      lineCut = 2,
      pointCut = 3,
      color = 5,
      swap = 3,
      merge = 1
    )
  }
}
