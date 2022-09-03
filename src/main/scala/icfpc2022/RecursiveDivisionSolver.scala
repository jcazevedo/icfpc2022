package icfpc2022

import java.io.File
import javax.imageio.ImageIO

import scala.collection.mutable

object RecursiveDivisionSolver extends Solver {
  val SubDivisions = 10
  val BeamSize = 1000
  val MaxIterations = 1000

  def solve(target: File): (Program, Long) = {
    val image = ImageIO.read(target)
    val width = image.getWidth()
    val height = image.getHeight()

    val scoreCache = mutable.Map.empty[Block, Double]
    val frequentColorCache = mutable.Map.empty[Shape, Color]

    def mostFrequentColor(shape: Shape): Color = {
      if (!frequentColorCache.contains(shape)) {
        val counts = mutable.Map.empty[Color, Int]

        (shape.bottomLeft.x until shape.topRight.x).foreach(x =>
          (shape.bottomLeft.y until shape.topRight.y).foreach(y => {
            val color = Color.fromInt(image.getRGB(x, height - y - 1))
            counts(color) = counts.getOrElse(color, 0) + 1
          })
        )

        frequentColorCache(shape) = counts.maxBy(_._2)._1
      }
      frequentColorCache(shape)
    }

    case class SearchNode(program: Program) {
      lazy val score = Scorer.score(program, image, scoreCache)
    }

    val start = SearchNode(Program(Canvas.blank(image.getWidth(), image.getHeight())))
    var best = start

    implicit val searchNodeOrdering: Ordering[SearchNode] =
      Ordering.by((node: SearchNode) => node.score).reverse

    var pq = mutable.PriorityQueue.empty[SearchNode]
    def enqueueState(state: SearchNode): Unit =
      pq.enqueue(state)

    enqueueState(start)

    println(s"Start score: ${start.score}")

    var iterations = 0
    while (pq.nonEmpty && iterations < MaxIterations) {
      val current = pq.dequeue()
      if (current.score < best.score) {
        println(s"New best score: ${current.score}")
        best = current
      }

      current.program.canvas.blocks.foreach { case (id, block) =>
        if (block.shape.width >= SubDivisions && block.shape.height >= SubDivisions) {
          val widthStep = math.max(block.shape.width / SubDivisions, 1)
          val heightStep = math.max(block.shape.height / SubDivisions, 1)

          (widthStep until block.shape.width by widthStep).foreach { w =>
            (heightStep until block.shape.height by heightStep).foreach { h =>
              val afterCut = Interpreter
                .unsafeApply(
                  current.program,
                  PointCutMove(
                    id,
                    Coords(
                      block.shape.bottomLeft.x + w,
                      block.shape.bottomLeft.y + h
                    )
                  )
                )
              val colorMoves = (0 until 4)
                .map(subId => ColorMove(s"$id.$subId", mostFrequentColor(afterCut.canvas.blocks(s"$id.$subId").shape)))
                .toList
              val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)
              enqueueState(SearchNode(afterColors))
            }
          }
        }

        if (block.shape.width >= SubDivisions) {
          val widthStep = math.max(block.shape.width / SubDivisions, 1)

          (widthStep until block.shape.width by widthStep).foreach { w =>
            val afterCut = Interpreter.unsafeApply(
              current.program,
              LineCutMove(id, LineCutMove.Vertical, block.shape.bottomLeft.x + w)
            )
            val colorMoves = (0 until 2)
              .map(subId => ColorMove(s"$id.$subId", mostFrequentColor(afterCut.canvas.blocks(s"$id.$subId").shape)))
              .toList
            val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)
            enqueueState(SearchNode(afterColors))
          }
        }

        if (block.shape.height >= SubDivisions) {
          val heightStep = math.max(block.shape.height / SubDivisions, 1)

          (heightStep until block.shape.height by heightStep).foreach { h =>
            val afterCut = Interpreter.unsafeApply(
              current.program,
              LineCutMove(id, LineCutMove.Horizontal, block.shape.bottomLeft.y + h)
            )
            val colorMoves = (0 until 2)
              .map(subId => ColorMove(s"$id.$subId", mostFrequentColor(afterCut.canvas.blocks(s"$id.$subId").shape)))
              .toList
            val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)
            enqueueState(SearchNode(afterColors))
          }
        }
      }

      if (pq.size > BeamSize)
        pq = pq.dropRight(pq.size - BeamSize)

      iterations += 1
      if (iterations % 100 == 0)
        println(s"Ran $iterations iterations...")
    }

    (best.program, best.score)
  }
}
