package icfpc2022

import java.awt.image.BufferedImage
import java.util.Comparator

import scala.collection.mutable

import com.google.common.collect.MinMaxPriorityQueue

object Solver {
  val BestCutBreadth = 5
  val BeamSize = 800000
  val MaxExpansions = 100000
  val ColorDiffTolerance = 1

  def solve(image: BufferedImage, initialProgram: Program): Program = {
    val timers = new Timers
    val height = image.getHeight()

    val scoreCache = mutable.Map.empty[(Shape, Color), Double]

    def getOriginalColor(x: Int, y: Int): Color =
      Color.fromInt(image.getRGB(x, height - y - 1))

    val mostFrequentColorCache = mutable.Map.empty[Shape, Color]
    def mostFrequentColor(shape: Shape): Color = {
      if (!mostFrequentColorCache.contains(shape)) {
        val counts = mutable.Map.empty[Color, Int]

        (shape.bottomLeft.x until shape.topRight.x).foreach(x =>
          (shape.bottomLeft.y until shape.topRight.y).foreach(y => {
            val color = getOriginalColor(x, y)
            counts(color) = counts.getOrElse(color, 0) + 1
          })
        )

        mostFrequentColorCache(shape) = counts.maxBy(_._2)._1
      }
      mostFrequentColorCache(shape)
    }

    val averageColorCache = mutable.Map.empty[Shape, Color]
    def averageColor(shape: Shape): Color = {
      if (!averageColorCache.contains(shape)) {
        var r = 0L
        var g = 0L
        var b = 0L
        var a = 0L

        (shape.bottomLeft.x until shape.topRight.x).foreach(x =>
          (shape.bottomLeft.y until shape.topRight.y).foreach(y => {
            val color = getOriginalColor(x, y)
            r += color.r
            g += color.g
            b += color.b
            a += color.a
          })
        )

        averageColorCache(shape) =
          Color((r / shape.size).toInt, (g / shape.size).toInt, (b / shape.size).toInt, (a / shape.size).toInt)
      }
      averageColorCache(shape)
    }

    def isSameColor(color1: Color, color2: Color): Boolean =
      Scorer.pixelDiff(color1, color2) < ColorDiffTolerance

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

    val bestPointCutsCache = mutable.Map.empty[Shape, List[Coords]]
    def bestPointCuts(shape: Shape): List[Coords] = {
      if (!bestPointCutsCache.contains(shape)) {
        val verticalCandidates =
          (1 until shape.width).map(w => w -> lineCutScore(shape, LineCutMove.Vertical, shape.bottomLeft.x + w))
        val horizontalCandidates =
          (1 until shape.height).map(h => h -> lineCutScore(shape, LineCutMove.Horizontal, shape.bottomLeft.y + h))

        val candidates = for {
          (w, vs) <- verticalCandidates.sortBy(_._2).reverse.take(BestCutBreadth)
          (h, hs) <- horizontalCandidates.sortBy(_._2).reverse.take(BestCutBreadth)
          score = (vs * shape.height + hs * shape.width) / (shape.height + shape.width)
        } yield (Coords(shape.bottomLeft.x + w, shape.bottomLeft.y + h) -> score)

        bestPointCutsCache(shape) =
          candidates.filter(_._2 > 0.0).sortBy(_._2).reverse.take(BestCutBreadth).map(_._1).toList
      }
      bestPointCutsCache(shape)
    }

    val singleBlockMovesCache = mutable.Map.empty[Block, List[SingleBlockMove]]
    def singleBlockMoves(id: String, block: Block): List[SingleBlockMove] = {
      if (!singleBlockMovesCache.contains(block)) {
        val moves = mutable.ListBuffer.empty[SingleBlockMove]

        // Try point cuts.
        if (block.shape.width > 1 && block.shape.height > 1) {
          timers.time("point cut expansion") {
            val bestCuts = bestPointCuts(block.shape)
            bestCuts.foreach(bestCut => moves.addOne(PointCutMove(id, bestCut)))
          }
        }

        // Try vertical cuts.
        if (block.shape.width > 1) {
          timers.time("vertical cut expansion") {
            val bestOffsets = bestLineCuts(block.shape, LineCutMove.Vertical)
            bestOffsets.foreach(bestOffset => moves.addOne(LineCutMove(id, LineCutMove.Vertical, bestOffset)))
          }
        }

        // Try horizontal cuts.
        if (block.shape.height > 1) {
          timers.time("horizontal cut expansion") {
            val bestOffsets = bestLineCuts(block.shape, LineCutMove.Horizontal)
            bestOffsets.foreach(bestOffset => moves.addOne(LineCutMove(id, LineCutMove.Horizontal, bestOffset)))
          }
        }

        // Try painting.
        timers.time("paint expansion") {
          moves.addOne(ColorMove(id, mostFrequentColor(block.shape)))
          moves.addOne(ColorMove(id, averageColor(block.shape)))
        }

        singleBlockMovesCache(block) = moves.toList
      }

      singleBlockMovesCache(block).map {
        case move: LineCutMove  => move.copy(blockId = id)
        case move: PointCutMove => move.copy(blockId = id)
        case move: ColorMove    => move.copy(blockId = id)
      }
    }

    case class SearchNode(program: Program) {
      lazy val similarity = Scorer.similarity(program, image, scoreCache)
      lazy val score = timers.time("score")(Scorer.score(program, image, scoreCache))
    }

    val start = SearchNode(initialProgram)
    var best = start

    val pq: MinMaxPriorityQueue[SearchNode] = MinMaxPriorityQueue
      .orderedBy(new Comparator[SearchNode] {
        def compare(o1: SearchNode, o2: SearchNode): Int =
          if (o1.score < o2.score) -1
          else if (o1.score > o2.score) 1
          else if (o1.similarity < o2.similarity) -1
          else if (o1.similarity > o2.similarity) 1
          else 0
      })
      .maximumSize(BeamSize)
      .create()
    val visited = mutable.Map.empty[Int, Long]
    def enqueueState(state: SearchNode): Unit = {
      timers.time("enqueuing") {
        val hash = state.program.canvas.simpleBlockSet.map(b => (b.shape, b.color)).hashCode()
        if (!visited.contains(hash) || visited(hash) > state.score) {
          visited(hash) = state.score
          pq.add(state)
        }
      }
    }

    enqueueState(start)

    println(s"Start score: ${start.score}")

    var expansions = 0
    var ts = System.currentTimeMillis()
    while (!pq.isEmpty && expansions < MaxExpansions) {
      timers.time("expansion") {
        val current = pq.poll()
        if (current.score < best.score)
          best = current

        def enqueueMove(move: Move): Unit = {
          val nextProgram = timers.time("apply move")(Interpreter.unsafeApply(current.program, move))
          enqueueState(SearchNode(nextProgram))
        }

        timers.time("expand node") {
          current.program.canvas.blockList.foreach { case (id, block) =>
            val singleMoves = timers.time("get single block moves")(singleBlockMoves(id, block))
            timers.time("expand moves") {
              singleMoves.foreach(enqueueMove)

              timers.time("expand multi-block moves") {
                current.program.canvas
                  .shapeSizeMap((block.shape.width, block.shape.height))
                  .dropWhile(_._1 != id)
                  .drop(1)
                  .foreach({ case (otherId, _) => enqueueMove(SwapMove(id, otherId)) })

                current.program.canvas.coordHeightMap
                  .get(
                    (Coords(block.shape.bottomLeft.x + block.shape.width, block.shape.bottomLeft.y), block.shape.height)
                  )
                  .foreach { case ((otherId, _)) =>
                    enqueueMove(MergeMove(id, otherId))
                  }

                current.program.canvas.coordWidthMap
                  .get(
                    (Coords(block.shape.bottomLeft.x, block.shape.bottomLeft.y + block.shape.height), block.shape.width)
                  )
                  .foreach { case ((otherId, _)) =>
                    enqueueMove(MergeMove(id, otherId))
                  }
              }
            }
          }
        }

        expansions += 1
        if (expansions % 100 == 0) {
          val diff = System.currentTimeMillis() - ts
          println(s"Ran $expansions expansions (the last 100 took ${diff}ms)...")
          println(s"Best so far: ${best.score} (${best.program.canvas.blocks.size} blocks)")
          println(s"Queue size: ${pq.size()}")
          println(s"Queue head score: ${pq.peek().score}")
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
