package icfpc2022

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.collection.mutable

import icfpc2022.syntax._

object Solver {
  private def pixelDiff(c1: Color, c2: Color): Double = {
    val r2 = (c1.r - c2.r).toDouble * (c1.r - c2.r).toDouble
    val g2 = (c1.g - c2.g).toDouble * (c1.g - c2.g).toDouble
    val b2 = (c1.b - c2.b).toDouble * (c1.b - c2.b).toDouble
    val a2 = (c1.a - c2.a).toDouble * (c1.a - c2.a).toDouble
    return math.sqrt(r2 + g2 + b2 + a2)
  }

  // This assumes all the canvas is filled and the images have the same size.
  private def similarity(
      p: Program,
      image: BufferedImage,
      cache: mutable.Map[Block, Double] = mutable.Map.empty[Block, Double]
  ): Long = {
    lazy val height = image.getHeight()

    def blockDiff(block: Block): Double = {
      if (!cache.contains(block)) {
        val res = block match {
          case SimpleBlock(shape, color) =>
            (shape.bottomLeft.x until shape.topRight.x)
              .map(x =>
                (shape.bottomLeft.y until shape.topRight.y)
                  .map(y => pixelDiff(color, Color.fromInt(image.getRGB(x, height - y - 1))))
                  .sum
              )
              .sum

          case ComplexBlock(_, childBlocks) =>
            childBlocks.map(blockDiff).sum
        }
        cache(block) = res
      }
      cache(block)
    }

    val diff = p.canvas.blocks.values.map(blockDiff).sum
    val alpha = 0.005

    math.round(diff * alpha)
  }

  private def validMoves(
      p: Program,
      image: BufferedImage,
      cache: mutable.Map[Block, Set[Move]],
      colorDiff: Int = 10000,
      resolution: Int = 10
  ): Set[Move] =
    p.canvas.blocks.flatMap { case (id, block) =>
      if (!cache.contains(block)) {
        val verticalCuts = ((block.shape.bottomLeft.x + 1) until block.shape.topRight.x by resolution)
          .map(xOffset => LineCutMove(id, LineCutMove.Vertical, xOffset))
          .toSet

        val horizontalCuts = ((block.shape.bottomLeft.y + 1) until block.shape.topRight.y by resolution)
          .map(yOffset => LineCutMove(id, LineCutMove.Horizontal, yOffset))
          .toSet

        val pointCuts = ((block.shape.bottomLeft.x + 1) until block.shape.topRight.x by resolution)
          .flatMap(x =>
            ((block.shape.bottomLeft.y + 1) until block.shape.topRight.y by resolution)
              .map(y => PointCutMove(id, Coords(x, y)))
              .toSet
          )
          .toSet

        val validColors = (block.shape.bottomLeft.x until block.shape.topRight.x)
          .flatMap(x =>
            (block.shape.bottomLeft.y until block.shape.topRight.y)
              .map(y => Color.fromInt(image.getRGB(x, image.getHeight() - y - 1)))
              .toSet
          )
          .toSet

        val colorMoves = validColors.map(ColorMove(id, _))

        cache(block) = verticalCuts ++ horizontalCuts ++ pointCuts ++ colorMoves
      }
      cache(block).map {
        case move: LineCutMove  => move.copy(blockId = id)
        case move: PointCutMove => move.copy(blockId = id)
        case move: ColorMove    => move.copy(blockId = id)
        case other              => other
      }
    }.toSet

  private def beamSearch(
      start: Program,
      image: BufferedImage,
      maxCandidates: Int = 100,
      maxExpansions: Int = 1000
  ): (Program, Long) = {
    val blockDiffCache = mutable.Map.empty[Block, Double]
    val movesCache = mutable.Map.empty[Block, Set[Move]]

    case class SearchNode(p: Program) {
      lazy val cost = p.cost + similarity(p, image, blockDiffCache)
    }
    object SearchNode {
      implicit val searchNodeOrdering: Ordering[SearchNode] =
        Ordering.by((node: SearchNode) => node.cost).reverse
    }

    val validColors = (0 until image.getWidth())
      .flatMap(x => (0 until image.getHeight()).map(y => Color.fromInt(image.getRGB(x, y))).toSet)
      .toSet

    var pq = mutable.PriorityQueue[SearchNode]()
    var best = SearchNode(start)
    pq.enqueue(best)

    var expansions = 0
    while (pq.nonEmpty && expansions < maxExpansions) {
      val current = pq.dequeue()
      val moves = validMoves(current.p, image, movesCache)

      println(s"Considering ${moves.size} moves")

      moves.foreach { move =>
        Interpreter.apply(current.p, move) match {
          case Left(value) =>
            println("ERROR!")
            println(value)
            println(current.p.isl)
            println(move.isl)
            println()

          case Right(nextP) =>
            val nextNode = SearchNode(nextP)
            if (nextNode.cost < best.cost)
              best = nextNode
            pq.enqueue(nextNode)
        }
      }

      if (pq.length > maxCandidates)
        pq = pq.dropRight(pq.length - maxCandidates)

      expansions += 1
    }

    (best.p, best.cost)
  }

  private def averageColor(program: Program, image: BufferedImage): (Program, Long) = {
    val width = image.getWidth()
    val height = image.getHeight()

    val colors = (0 until width).flatMap(x => (0 until height).map(y => Color.fromInt(image.getRGB(x, y))))

    val (totR, totG, totB, totA) = colors.foldLeft((0L, 0L, 0L, 0L)) { case ((ra, ga, ba, aa), Color(r, g, b, a)) =>
      (ra + r, ga + g, ba + b, aa + a)
    }

    val next = Interpreter
      .apply(
        program,
        ColorMove(
          "0",
          Color(
            (totR / colors.length).toInt,
            (totG / colors.length).toInt,
            (totB / colors.length).toInt,
            (totA / colors.length).toInt
          )
        )
      )
      .toOption
      .get

    (next, next.cost + similarity(next, image))
  }

  def solve(target: File): (Program, Long) = {
    val image = ImageIO.read(target)
    val start = Program(Canvas.blank(image.getWidth(), image.getHeight()))
    // beamSearch(start, image)
    averageColor(start, image)
  }
}
