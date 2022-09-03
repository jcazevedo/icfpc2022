package icfpc2022

import java.awt.image.BufferedImage

import scala.collection.mutable

object Solver {
  val BestCutBreadth = 5
  val BeamSize = 10000
  val MaxIterations = 1000

  def solve(image: BufferedImage, initialCanvas: Canvas): Program = {
    val width = image.getWidth()
    val height = image.getHeight()

    val scoreCache = mutable.Map.empty[Block, Double]
    val frequentColorCache = mutable.Map.empty[Shape, Color]

    def getOriginalColor(x: Int, y: Int): Color =
      Color.fromInt(image.getRGB(x, height - y - 1))

    def mostFrequentColor(shape: Shape): Color = {
      if (!frequentColorCache.contains(shape)) {
        val counts = mutable.Map.empty[Color, Int]

        (shape.bottomLeft.x until shape.topRight.x).foreach(x =>
          (shape.bottomLeft.y until shape.topRight.y).foreach(y => {
            val color = getOriginalColor(x, y)
            counts(color) = counts.getOrElse(color, 0) + 1
          })
        )

        frequentColorCache(shape) = counts.maxBy(_._2)._1
      }
      frequentColorCache(shape)
    }

    def isSameColor(color1: Color, color2: Color): Boolean =
      Scorer.pixelDiff(color1, color2) < 30

    val pointCutDifferencesCache = mutable.Map.empty[(Shape, Coords), Int]

    def pointCutDifferences(shape: Shape, coords: Coords): Int = {
      if (!pointCutDifferencesCache.contains((shape, coords))) {
        var sameColor = 0
        var differentColor = 0
        var totDiff = shape.width + shape.height

        (shape.bottomLeft.x until shape.topRight.x).foreach { x =>
          val up = getOriginalColor(x, coords.y)
          val down = getOriginalColor(x, coords.y - 1)
          if (isSameColor(up, down)) sameColor += 1
          else differentColor += 1
        }

        (shape.bottomLeft.y until shape.topRight.y).foreach { y =>
          val left = getOriginalColor(coords.x - 1, y)
          val right = getOriginalColor(coords.x, y)
          if (isSameColor(left, right)) sameColor += 1
          else differentColor += 1
        }

        pointCutDifferencesCache((shape, coords)) = differentColor
      }
      pointCutDifferencesCache((shape, coords))
    }

    val lineCutDifferencesCache = mutable.Map.empty[(Shape, LineCutMove.Orientation, Int), Int]

    def lineCutDifferences(shape: Shape, orientation: LineCutMove.Orientation, offset: Int) = {
      if (!lineCutDifferencesCache.contains((shape, orientation, offset))) {
        var sameColor = 0
        var differentColor = 0

        orientation match {
          case LineCutMove.Horizontal =>
            (shape.bottomLeft.x until shape.topRight.x).foreach { x =>
              val up = getOriginalColor(x, offset)
              val down = getOriginalColor(x, offset - 1)
              if (isSameColor(up, down)) sameColor += 1
              else differentColor += 1
            }

          case LineCutMove.Vertical =>
            (shape.bottomLeft.y until shape.topRight.y).foreach { y =>
              val left = getOriginalColor(offset - 1, y)
              val right = getOriginalColor(offset, y)
              if (isSameColor(left, right)) sameColor += 1
              else differentColor += 1
            }
        }

        lineCutDifferencesCache((shape, orientation, offset)) = differentColor
      }
      lineCutDifferencesCache((shape, orientation, offset))
    }

    case class SearchNode(program: Program) {
      lazy val similarity = Scorer.similarity(program, image, scoreCache)
      lazy val score = Scorer.score(program, image, scoreCache)
    }

    val start = SearchNode(Program.fromInitialCanvas(initialCanvas))
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
        if (block.shape.width > 1 && block.shape.height > 1) {
          val candidates = (1 until block.shape.width).flatMap { w =>
            (1 until block.shape.height).map { h =>
              val cutCoords = Coords(block.shape.bottomLeft.x + w, block.shape.bottomLeft.y + h)
              cutCoords -> pointCutDifferences(block.shape, cutCoords)
            }
          }

          val bestCuts = candidates.filter(_._2 > 0).sortBy(_._2).reverse.take(BestCutBreadth).map(_._1)

          bestCuts.foreach { bestCut =>
            val afterCut = Interpreter.unsafeApply(
              current.program,
              PointCutMove(id, bestCut)
            )
            val colorMoves = (0 until 4).flatMap { subId =>
              val targetBlock = afterCut.canvas.blocks(s"$id.$subId")
              val targetColor = mostFrequentColor(targetBlock.shape)
              targetBlock match {
                case ComplexBlock(shape, childBlocks) if childBlocks.exists(b => !isSameColor(b.color, targetColor)) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case SimpleBlock(shape, color) if !isSameColor(color, targetColor) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case _ => None
              }
            }.toList
            val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)
            enqueueState(SearchNode(afterColors))
          }
        }

        if (block.shape.width > 1) {
          val candidates = (1 until block.shape.width).map { w =>
            val offset = block.shape.bottomLeft.x + w
            offset -> lineCutDifferences(block.shape, LineCutMove.Vertical, offset)
          }

          val bestOffsets = candidates.filter(_._2 > 0).sortBy(_._2).reverse.take(BestCutBreadth).map(_._1)

          bestOffsets.foreach { bestOffset =>
            val afterCut = Interpreter.unsafeApply(
              current.program,
              LineCutMove(id, LineCutMove.Vertical, bestOffset)
            )
            val colorMoves = (0 until 2).flatMap { subId =>
              val targetBlock = afterCut.canvas.blocks(s"$id.$subId")
              val targetColor = mostFrequentColor(targetBlock.shape)
              targetBlock match {
                case ComplexBlock(shape, childBlocks) if childBlocks.exists(b => !isSameColor(b.color, targetColor)) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case SimpleBlock(shape, color) if !isSameColor(color, targetColor) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case _ => None
              }
            }.toList
            val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)
            enqueueState(SearchNode(afterColors))
          }
        }

        if (block.shape.height > 1) {
          val candidates = (1 until block.shape.height).map { h =>
            val offset = block.shape.bottomLeft.y + h
            offset -> lineCutDifferences(block.shape, LineCutMove.Horizontal, offset)
          }

          val bestOffsets = candidates.filter(_._2 > 0).sortBy(_._2).reverse.take(BestCutBreadth).map(_._1)

          bestOffsets.foreach { bestOffset =>
            val afterCut = Interpreter.unsafeApply(
              current.program,
              LineCutMove(id, LineCutMove.Horizontal, bestOffset)
            )
            val colorMoves = (0 until 2).flatMap { subId =>
              val targetBlock = afterCut.canvas.blocks(s"$id.$subId")
              val targetColor = mostFrequentColor(targetBlock.shape)
              targetBlock match {
                case ComplexBlock(shape, childBlocks) if childBlocks.exists(b => !isSameColor(b.color, targetColor)) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case SimpleBlock(shape, color) if !isSameColor(color, targetColor) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case _ => None
              }
            }.toList
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

    best.program
  }
}
