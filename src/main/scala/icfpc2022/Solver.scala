package icfpc2022

import java.awt.image.BufferedImage

import scala.collection.mutable

object Solver {
  val BestCutBreadth = 5
  val BeamSize = 100
  val MaxExpansions = 1000
  val ColorDiffTolerance = 30

  def solve(image: BufferedImage, initialCanvas: Canvas): Program = {
    val timers = new Timers
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

    val pointCutScoreCache = mutable.Map.empty[(Shape, Coords), Double]
    def pointCutScore(shape: Shape, coords: Coords): Double = {
      if (!pointCutScoreCache.contains((shape, coords))) {
        var sameColor = 0.0
        var differentColor = 0.0

        (shape.bottomLeft.x until shape.topRight.x).foreach { x =>
          val up = getOriginalColor(x, coords.y)
          val down = getOriginalColor(x, coords.y - 1)
          if (isSameColor(up, down)) sameColor += 1.0
          else differentColor += 1.0
        }

        (shape.bottomLeft.y until shape.topRight.y).foreach { y =>
          val left = getOriginalColor(coords.x - 1, y)
          val right = getOriginalColor(coords.x, y)
          if (isSameColor(left, right)) sameColor += 1.0
          else differentColor += 1.0
        }

        pointCutScoreCache((shape, coords)) = differentColor / (sameColor + differentColor)
      }
      pointCutScoreCache((shape, coords))
    }

    val bestPointCutsCache = mutable.Map.empty[Shape, List[Coords]]

    def bestPointCuts(shape: Shape): List[Coords] = {
      if (!bestPointCutsCache.contains(shape)) {
        val candidates = (1 until shape.width).flatMap { w =>
          (1 until shape.height).map { h =>
            val cutCoords = Coords(shape.bottomLeft.x + w, shape.bottomLeft.y + h)
            cutCoords -> pointCutScore(shape, cutCoords)
          }
        }
        bestPointCutsCache(shape) =
          candidates.filter(_._2 > 0.0).sortBy(_._2).reverse.take(BestCutBreadth).map(_._1).toList
      }
      bestPointCutsCache(shape)
    }

    val lineCutScoreCache = mutable.Map.empty[(Shape, LineCutMove.Orientation, Int), Double]
    def lineCutScore(shape: Shape, orientation: LineCutMove.Orientation, offset: Int): Double = {
      if (!lineCutScoreCache.contains((shape, orientation, offset))) {
        var sameColor = 0.0
        var differentColor = 0.0

        orientation match {
          case LineCutMove.Horizontal =>
            (shape.bottomLeft.x until shape.topRight.x).foreach { x =>
              val up = getOriginalColor(x, offset)
              val down = getOriginalColor(x, offset - 1)
              if (isSameColor(up, down)) sameColor += 1.0
              else differentColor += 1.0
            }

          case LineCutMove.Vertical =>
            (shape.bottomLeft.y until shape.topRight.y).foreach { y =>
              val left = getOriginalColor(offset - 1, y)
              val right = getOriginalColor(offset, y)
              if (isSameColor(left, right)) sameColor += 1.0
              else differentColor += 1.0
            }
        }

        lineCutScoreCache((shape, orientation, offset)) = differentColor / (sameColor + differentColor)
      }
      lineCutScoreCache((shape, orientation, offset))
    }

    val bestLineCutsCache = mutable.Map.empty[(Shape, LineCutMove.Orientation), List[Int]]

    def bestLineCuts(shape: Shape, orientation: LineCutMove.Orientation): List[Int] = {
      if (!bestLineCutsCache.contains((shape, orientation))) {
        val candidates = orientation match {
          case LineCutMove.Vertical =>
            (1 until shape.width).map { w =>
              val offset = shape.bottomLeft.x + w
              offset -> lineCutScore(shape, LineCutMove.Vertical, offset)
            }

          case LineCutMove.Horizontal =>
            (1 until shape.height).map { h =>
              val offset = shape.bottomLeft.y + h
              offset -> lineCutScore(shape, LineCutMove.Horizontal, offset)
            }
        }

        bestLineCutsCache((shape, orientation)) =
          candidates.filter(_._2 > 0.0).sortBy(_._2).reverse.take(BestCutBreadth).map(_._1).toList
      }
      bestLineCutsCache((shape, orientation))
    }

    case class SearchNode(program: Program) {
      lazy val similarity = Scorer.similarity(program, image, scoreCache)
      lazy val score = timers.time("score")(Scorer.score(program, image, scoreCache))
    }

    def postProcess(state: SearchNode): SearchNode =
      state

    val start = SearchNode(Program.fromInitialCanvas(initialCanvas))
    var best = start

    implicit val searchNodeOrdering: Ordering[SearchNode] =
      Ordering.by((node: SearchNode) => node.score).reverse

    var pq = mutable.PriorityQueue.empty[SearchNode]
    val visited = mutable.Map.empty[Int, Long]
    def enqueueState(state: SearchNode): Unit = {
      timers.time("enqueuing") {
        val postProcessedState = postProcess(state)
        val hash = postProcessedState.program.canvas.simpleBlockSet.map(b => (b.shape, b.color)).hashCode()
        if (!visited.contains(hash) || visited(hash) > postProcessedState.score) {
          visited(hash) = state.score
          pq.enqueue(postProcessedState)
        }
      }
    }

    enqueueState(start)

    println(s"Start score: ${start.score}")

    var expansions = 0
    var ts = System.currentTimeMillis()
    while (pq.nonEmpty && expansions < MaxExpansions) {
      timers.time("expansion") {
        val current = pq.dequeue()
        if (current.score < best.score)
          best = current

        current.program.canvas.blocks.foreach { case (id, block) =>
          // Try point cuts.
          if (block.shape.width > 1 && block.shape.height > 1) {
            timers.time("point cut expansion") {
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
          }

          // Try vertical cuts.
          if (block.shape.width > 1) {
            timers.time("vertical cut expansion") {
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
          }

          // Try horizontal cuts.
          if (block.shape.height > 1) {
            timers.time("horizontal cut expansion") {
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

          // Try painting.
          timers.time("paint expansion") {
            val targetColor = mostFrequentColor(block.shape)
            val shouldPaint = block match {
              case ComplexBlock(_, blocks) => blocks.exists(b => !isSameColor(b.color, targetColor))
              case SimpleBlock(_, color)   => !isSameColor(color, targetColor)
            }
            if (shouldPaint) {
              val afterPaint = Interpreter.unsafeApply(current.program, ColorMove(id, targetColor))
              enqueueState(SearchNode(afterPaint))
            }
          }
        }

        if (pq.size > BeamSize) {
          timers.time("beam cut") {
            pq = pq.take(BeamSize)
          }
        }

        expansions += 1
        if (expansions % 100 == 0) {
          val diff = System.currentTimeMillis() - ts
          println(s"Ran $expansions expansions (the last 100 took ${diff}ms)...")
          println(s"Best so far: ${best.score}")
          if (pq.nonEmpty)
            println(s"Next node score: ${pq.head.score}")
          ts = System.currentTimeMillis()
          println()
          timers.outputAll
          println()
          timers.reset()
        }
      }
    }

    best.program
  }
}
