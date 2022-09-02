package icfpc2022

import java.io.File
import javax.imageio.ImageIO

import scala.collection.mutable

object RecursiveDivisionSolver extends Solver {
  val BeamSize = 100
  val MaxIterations = 1000

  def solve(target: File): (Program, Long) = {
    val image = ImageIO.read(target)
    val width = image.getWidth()
    val height = image.getHeight()

    def mostFrequentColor(shape: Shape): Color = {
      val counts = mutable.Map.empty[Color, Int]

      (shape.bottomLeft.x until shape.topRight.x).foreach(x =>
        (shape.bottomLeft.y until shape.topRight.y).foreach(y => {
          val color = Color.fromInt(image.getRGB(x, height - y - 1))
          counts(color) = counts.getOrElse(color, 0) + 1
        })
      )

      counts.maxBy(_._2)._1
    }

    val scoreCache = mutable.Map.empty[Block, Double]

    case class SearchNode(program: Program) {
      lazy val score = Scorer.score(program, image, scoreCache)
    }

    val start = SearchNode(Program(Canvas.blank(image.getWidth(), image.getHeight())))
    var best = start

    implicit val searchNodeOrdering: Ordering[SearchNode] =
      Ordering.by((node: SearchNode) => node.score).reverse

    var pq = mutable.PriorityQueue.empty[SearchNode]
    pq.enqueue(start)

    var iterations = 0
    while (pq.nonEmpty && iterations < MaxIterations) {
      val current = pq.dequeue()
      if (current.score < best.score)
        best = current

      current.program.canvas.blocks.foreach { case (id, block) =>
        if (block.shape.width > 1 && block.shape.height > 1) {
          val afterCut = Interpreter
            .unsafeApply(
              current.program,
              PointCutMove(
                id,
                Coords(
                  block.shape.bottomLeft.x + block.shape.width / 2,
                  block.shape.bottomLeft.y + block.shape.height / 2
                )
              )
            )

          val colorMoves = (0 until 4)
            .map(subId => ColorMove(s"$id.$subId", mostFrequentColor(afterCut.canvas.blocks(s"$id.$subId").shape)))
            .toList

          val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)

          pq.enqueue(SearchNode(afterColors))
        }

      // if (block.shape.width > 1) {
      //   val afterCut = Interp
      // }
      }

      if (pq.size > BeamSize)
        pq = pq.dropRight(pq.size - BeamSize)

      iterations += 1
    }

    (best.program, best.score)
  }
}
