package icfpc2022

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.collection.mutable

object Solver {
  val BestCutBreadth = 5
  val BeamSize = 1000
  val MaxExpansions = 2000
  val ColorDiffTolerance = 30

  def solve(image: BufferedImage, initialCanvas: Canvas): Program = {
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
      Scorer.pixelDiff(color1, color2) < ColorDiffTolerance

    val pointCutDifferencesCache = mutable.Map.empty[(Shape, Coords), Int]

    def pointCutDifferences(shape: Shape, coords: Coords): Int = {
      if (!pointCutDifferencesCache.contains((shape, coords))) {
        var sameColor = 0
        var differentColor = 0

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

    val bestPointCutsCache = mutable.Map.empty[Shape, List[Coords]]

    def bestPointCuts(shape: Shape): List[Coords] = {
      if (!bestPointCutsCache.contains(shape)) {
        val candidates = (1 until shape.width).flatMap { w =>
          (1 until shape.height).map { h =>
            val cutCoords = Coords(shape.bottomLeft.x + w, shape.bottomLeft.y + h)
            cutCoords -> pointCutDifferences(shape, cutCoords)
          }
        }
        bestPointCutsCache(shape) =
          candidates.filter(_._2 > 0).sortBy(_._2).reverse.take(BestCutBreadth).map(_._1).toList
      }
      bestPointCutsCache(shape)
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

    val bestLineCutsCache = mutable.Map.empty[(Shape, LineCutMove.Orientation), List[Int]]

    def bestLineCuts(shape: Shape, orientation: LineCutMove.Orientation): List[Int] = {
      if (!bestLineCutsCache.contains((shape, orientation))) {
        val candidates = orientation match {
          case LineCutMove.Vertical =>
            (1 until shape.width).map { w =>
              val offset = shape.bottomLeft.x + w
              offset -> lineCutDifferences(shape, LineCutMove.Vertical, offset)
            }

          case LineCutMove.Horizontal =>
            (1 until shape.height).map { h =>
              val offset = shape.bottomLeft.y + h
              offset -> lineCutDifferences(shape, LineCutMove.Horizontal, offset)
            }
        }

        bestLineCutsCache((shape, orientation)) =
          candidates.filter(_._2 > 0).sortBy(_._2).reverse.take(BestCutBreadth).map(_._1).toList
      }
      bestLineCutsCache((shape, orientation))
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

    val visited = mutable.Map.empty[Int, Long]
    def enqueueState(state: SearchNode): Unit = {
      val hash = state.program.canvas.simpleBlockSet.map(b => (b.shape, b.color)).hashCode()
      if (!visited.contains(hash) || visited(hash) > state.score) {
        visited(hash) = state.score
        pq.enqueue(state)
      }
    }

    enqueueState(start)

    println(s"Start score: ${start.score}")

    var expansions = 0
    var ts = System.currentTimeMillis()
    while (pq.nonEmpty && expansions < MaxExpansions) {
      val current = pq.dequeue()
      if (current.score < best.score) {
        println(s"New best score: ${current.score}")
        best = current
      }

      current.program.canvas.blocks.foreach { case (id, block) =>
        // Try point cuts.
        if (block.shape.width > 1 && block.shape.height > 1) {
          val bestCuts = bestPointCuts(block.shape)

          bestCuts.foreach { bestCut =>
            val afterCut = Interpreter.unsafeApply(
              current.program,
              PointCutMove(id, bestCut)
            )
            val colorMoves = (0 until 4).flatMap { subId =>
              val targetBlock = afterCut.canvas.blocks(s"$id.$subId")
              val targetColor = mostFrequentColor(targetBlock.shape)
              targetBlock match {
                case ComplexBlock(_, childBlocks) if childBlocks.exists(b => !isSameColor(b.color, targetColor)) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case SimpleBlock(_, color) if !isSameColor(color, targetColor) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case _ => None
              }
            }.toList
            val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)
            enqueueState(SearchNode(afterColors))
          }
        }

        // Try vertical cuts.
        if (block.shape.width > 1) {
          val bestOffsets = bestLineCuts(block.shape, LineCutMove.Vertical)

          bestOffsets.foreach { bestOffset =>
            val afterCut = Interpreter.unsafeApply(
              current.program,
              LineCutMove(id, LineCutMove.Vertical, bestOffset)
            )
            val colorMoves = (0 until 2).flatMap { subId =>
              val targetBlock = afterCut.canvas.blocks(s"$id.$subId")
              val targetColor = mostFrequentColor(targetBlock.shape)
              targetBlock match {
                case ComplexBlock(_, childBlocks) if childBlocks.exists(b => !isSameColor(b.color, targetColor)) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case SimpleBlock(_, color) if !isSameColor(color, targetColor) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case _ => None
              }
            }.toList
            val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)
            enqueueState(SearchNode(afterColors))
          }
        }

        // Try horizontal cuts.
        if (block.shape.height > 1) {
          val bestOffsets = bestLineCuts(block.shape, LineCutMove.Horizontal)

          bestOffsets.foreach { bestOffset =>
            val afterCut = Interpreter.unsafeApply(
              current.program,
              LineCutMove(id, LineCutMove.Horizontal, bestOffset)
            )
            val colorMoves = (0 until 2).flatMap { subId =>
              val targetBlock = afterCut.canvas.blocks(s"$id.$subId")
              val targetColor = mostFrequentColor(targetBlock.shape)
              targetBlock match {
                case ComplexBlock(_, childBlocks) if childBlocks.exists(b => !isSameColor(b.color, targetColor)) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case SimpleBlock(_, color) if !isSameColor(color, targetColor) =>
                  Some(ColorMove(s"$id.$subId", targetColor))
                case _ => None
              }
            }.toList
            val afterColors = Interpreter.unsafeApply(afterCut, colorMoves)
            enqueueState(SearchNode(afterColors))
          }
        }
      }

      // Try color (every block that we should color).
      val afterPaint = current.program.canvas.blocks.foldLeft(current.program) { case (program, (id, block)) =>
        val targetColor = mostFrequentColor(block.shape)
        val shouldPaint = block match {
          case ComplexBlock(_, blocks) => blocks.exists(b => !isSameColor(b.color, targetColor))
          case SimpleBlock(_, color)   => !isSameColor(color, targetColor)
        }
        if (shouldPaint) Interpreter.unsafeApply(program, ColorMove(id, targetColor))
        else program
      }
      enqueueState(SearchNode(afterPaint))

      // Try swaps (every block that it makes sense to swap).
      val (afterSwap, _) = current.program.canvas.blocks.foldLeft((current.program, Set.empty[String])) {
        case ((program, swapped), (id, block)) =>
          lazy val targetColor = mostFrequentColor(block.shape)
          lazy val shouldSwap = !swapped.contains(id) && (block match {
            case ComplexBlock(_, blocks) => blocks.exists(b => !isSameColor(b.color, targetColor))
            case SimpleBlock(_, color)   => !isSameColor(color, targetColor)
          })

          if (shouldSwap) {
            val targetForSwap = current.program.canvas.blocks.find { case (swapId, swapBlock) =>
              id != swapId && !swapped.contains(
                swapId
              ) && swapBlock.shape.width == block.shape.width && swapBlock.shape.height == block.shape.height && (swapBlock match {
                case ComplexBlock(_, blocks) => blocks.exists(b => isSameColor(b.color, targetColor))
                case SimpleBlock(_, color)   => isSameColor(color, targetColor)
              })
            }
            targetForSwap match {
              case None =>
                (program, swapped)
              case Some((swapId, _)) =>
                (Interpreter.unsafeApply(program, SwapMove(id, swapId)), swapped ++ Set(id, swapId))
            }
          } else
            (program, swapped)
      }
      enqueueState(SearchNode(afterSwap))
      if (expansions == 0)
        ImageIO.write(Interpreter.paint(afterSwap), "png", new File("test.png"))

      if (pq.size > BeamSize)
        pq = pq.dropRight(pq.size - BeamSize)

      expansions += 1
      if (expansions % 100 == 0) {
        val diff = System.currentTimeMillis() - ts
        println(s"Ran $expansions expansions (the last 100 took ${diff}ms)...")
        ts = System.currentTimeMillis()
      }
    }

    best.program
  }
}
