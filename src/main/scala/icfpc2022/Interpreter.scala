package icfpc2022

import java.awt.image.BufferedImage

import icfpc2022.LineCutMove.{Horizontal, Vertical}
import icfpc2022.syntax._

object Interpreter {
  object Errors {
    def unknownBlockId(id: String): String =
      s"Unknown block id $id"

    def unknownBlockIds(id1: String, id2: String): String =
      s"Unknown block ids $id1 and $id2"

    def offsetOutOfBounds(id: String, orientation: LineCutMove.Orientation, offset: Int) =
      s"Offset out of bounds of block id $id with orientation ${orientation.isl} and offset $offset"

    def offsetOutOfBounds(id: String, coords: Coords) =
      s"Offset out of bounds of block id $id with offset ${coords.isl}"

    def incompatibleShapes(id1: String, id2: String): String =
      s"Blocks with id $id1 and $id2 have incompatible shape"

    def nonAdjointBlocks(id1: String, id2: String): String =
      s"Blocks with id $id1 and $id2 are not adjoint"
  }

  private def costOf(move: Move, canvasShape: Shape, blockShape: Shape): Long = {
    val dynamicCost = canvasShape.size.toDouble / blockShape.size.toDouble
    val baseCost = move match {
      case _: ColorMove    => 5.0
      case _: LineCutMove  => 7.0
      case _: PointCutMove => 10.0
      case _: MergeMove    => 1.0
      case _: SwapMove     => 3.0
    }

    math.round(baseCost * dynamicCost)
  }

  private def canCutLine(shape: Shape, orientation: LineCutMove.Orientation, offset: Int): Boolean =
    orientation match {
      case LineCutMove.Vertical   => offset > shape.bottomLeft.x && offset < shape.topRight.x
      case LineCutMove.Horizontal => offset > shape.bottomLeft.y && offset < shape.topRight.y
    }

  private def cutLine(shape: Shape, orientation: LineCutMove.Orientation, offset: Int): Option[(Shape, Shape)] =
    if (canCutLine(shape, orientation, offset))
      Some(orientation match {
        case LineCutMove.Vertical =>
          (
            Shape(shape.bottomLeft, offset - shape.bottomLeft.x, shape.height),
            Shape(Coords(offset, shape.bottomLeft.y), shape.topRight.x - offset, shape.height)
          )
        case LineCutMove.Horizontal =>
          (
            Shape(shape.bottomLeft, shape.width, offset - shape.bottomLeft.y),
            Shape(Coords(shape.bottomLeft.x, offset), shape.width, shape.topRight.y - offset)
          )
      })
    else None

  private def cutPoint(shape: Shape, offset: Coords): Option[(Shape, Shape, Shape, Shape)] =
    for {
      (hCut03, hCut12) <- cutLine(shape, LineCutMove.Vertical, offset.x)
      (cut0, cut3) <- cutLine(hCut03, LineCutMove.Horizontal, offset.y)
      (cut1, cut2) <- cutLine(hCut12, LineCutMove.Horizontal, offset.y)
    } yield (cut0, cut1, cut2, cut3)

  private def offset(block: Block, bottomLeft: Coords): Block =
    block match {
      case ComplexBlock(shape, childBlocks) =>
        val diffX = shape.bottomLeft.x - bottomLeft.x
        val diffY = shape.bottomLeft.y - bottomLeft.y
        ComplexBlock(
          Shape(bottomLeft, shape.width, shape.height),
          childBlocks.map { case SimpleBlock(shape, color) =>
            SimpleBlock(
              Shape(Coords(shape.bottomLeft.x - diffX, shape.bottomLeft.y - diffY), shape.width, shape.height),
              color
            )
          }
        )

      case SimpleBlock(shape, color) =>
        SimpleBlock(Shape(bottomLeft, shape.width, shape.height), color)
    }

  def apply(program: Program, move: Move): Either[String, Program] = {
    lazy val nextMoves = move :: program.moves

    move match {
      case move @ ColorMove(blockId, color) =>
        program.canvas.blocks.get(blockId) match {
          case None =>
            Left(Errors.unknownBlockId(blockId))

          case Some(block) =>
            val nextBlocks = program.canvas.blocks.updated(blockId, SimpleBlock(block.shape, color))
            val nextCost = program.cost + costOf(move, program.canvas.shape, block.shape)

            Right(
              program.copy(
                canvas = program.canvas.copy(blocks = nextBlocks),
                moves = nextMoves,
                cost = nextCost
              )
            )
        }

      case LineCutMove(blockId, orientation, offset) =>
        program.canvas.blocks.get(blockId) match {
          case None =>
            Left(Errors.unknownBlockId(blockId))

          case Some(SimpleBlock(shape, color)) =>
            cutLine(shape, orientation, offset) match {
              case None =>
                Left(Errors.offsetOutOfBounds(blockId, orientation, offset))

              case Some((shape0, shape1)) =>
                val nextBlocks = program.canvas.blocks
                  .removed(blockId)
                  .updated(blockId + ".0", SimpleBlock(shape0, color))
                  .updated(blockId + ".1", SimpleBlock(shape1, color))
                val nextCost = program.cost + costOf(move, program.canvas.shape, shape)

                Right(
                  program.copy(
                    canvas = program.canvas.copy(blocks = nextBlocks),
                    moves = nextMoves,
                    cost = nextCost
                  )
                )
            }

          case Some(ComplexBlock(shape, blocks)) =>
            cutLine(shape, orientation, offset) match {
              case None =>
                Left(Errors.offsetOutOfBounds(blockId, orientation, offset))

              case Some((shape0, shape1)) =>
                val (split0, split1) = blocks.foldLeft((Set.empty[SimpleBlock], Set.empty[SimpleBlock])) {
                  case (acc @ (s0, s1), curr) =>
                    cutLine(curr.shape, orientation, offset) match {
                      case None => acc
                      case Some((cutShape0, cutShape1)) =>
                        (s0 + SimpleBlock(cutShape0, curr.color), s1 + SimpleBlock(cutShape1, curr.color))
                    }
                }

                val forBlockId0 = blocks.filter(_.shape.within(shape0)) ++ split0
                val forBlockId1 = blocks.filter(_.shape.within(shape1)) ++ split1

                val nextBlocks = program.canvas.blocks
                  .removed(blockId)
                  .updated(blockId + ".0", ComplexBlock(shape0, forBlockId0))
                  .updated(blockId + ".1", ComplexBlock(shape1, forBlockId1))
                val nextCost = program.cost + costOf(move, program.canvas.shape, shape)

                Right(
                  program.copy(
                    canvas = program.canvas.copy(blocks = nextBlocks),
                    moves = nextMoves,
                    cost = nextCost
                  )
                )
            }
        }

      case PointCutMove(blockId, offset) =>
        program.canvas.blocks.get(blockId) match {
          case None =>
            Left(Errors.unknownBlockId(blockId))

          case Some(SimpleBlock(shape, color)) =>
            cutPoint(shape, offset) match {
              case None =>
                Left(Errors.offsetOutOfBounds(blockId, offset))

              case Some((shape0, shape1, shape2, shape3)) =>
                val nextBlocks = program.canvas.blocks
                  .removed(blockId)
                  .updated(blockId + ".0", SimpleBlock(shape0, color))
                  .updated(blockId + ".1", SimpleBlock(shape1, color))
                  .updated(blockId + ".2", SimpleBlock(shape2, color))
                  .updated(blockId + ".3", SimpleBlock(shape3, color))
                val nextCost = program.cost + costOf(move, program.canvas.shape, shape)

                Right(
                  program.copy(
                    canvas = program.canvas.copy(blocks = nextBlocks),
                    moves = nextMoves,
                    cost = nextCost
                  )
                )
            }

          case Some(ComplexBlock(shape, blocks)) =>
            cutPoint(shape, offset) match {
              case None =>
                Left(Errors.offsetOutOfBounds(blockId, offset))

              case Some((shape0, shape1, shape2, shape3)) =>
                val (split0, split1, split2, split3) = blocks.foldLeft(
                  (Set.empty[SimpleBlock], Set.empty[SimpleBlock], Set.empty[SimpleBlock], Set.empty[SimpleBlock])
                ) { case (acc @ (s0, s1, s2, s3), curr) =>
                  cutPoint(curr.shape, offset) match {
                    case None => acc
                    case Some((cutShape0, cutShape1, cutShape2, cutShape3)) =>
                      (
                        s0 + SimpleBlock(cutShape0, curr.color),
                        s1 + SimpleBlock(cutShape1, curr.color),
                        s2 + SimpleBlock(cutShape2, curr.color),
                        s3 + SimpleBlock(cutShape3, curr.color)
                      )
                  }
                }

                val forBlock0 = blocks.filter(_.shape.within(shape0)) ++ split0
                val forBlock1 = blocks.filter(_.shape.within(shape1)) ++ split1
                val forBlock2 = blocks.filter(_.shape.within(shape2)) ++ split2
                val forBlock3 = blocks.filter(_.shape.within(shape3)) ++ split3

                val nextBlocks = program.canvas.blocks
                  .removed(blockId)
                  .updated(blockId + ".0", ComplexBlock(shape0, forBlock0))
                  .updated(blockId + ".1", ComplexBlock(shape1, forBlock1))
                  .updated(blockId + ".2", ComplexBlock(shape2, forBlock2))
                  .updated(blockId + ".3", ComplexBlock(shape3, forBlock3))
                val nextCost = program.cost + costOf(move, program.canvas.shape, shape)

                Right(
                  program.copy(canvas = program.canvas.copy(blocks = nextBlocks), moves = nextMoves, cost = nextCost)
                )
            }
        }

      case MergeMove(blockId1, blockId2) =>
        (program.canvas.blocks.get(blockId1), program.canvas.blocks.get(blockId2)) match {
          case (None, None) =>
            Left(Errors.unknownBlockIds(blockId1, blockId2))

          case (None, _) =>
            Left(Errors.unknownBlockId(blockId1))

          case (_, None) =>
            Left(Errors.unknownBlockId(blockId2))

          case (Some(block1), Some(block2)) =>
            val List(smallest, biggest) = List(block1.shape, block2.shape).sorted

            lazy val block1Blocks = block1 match {
              case ComplexBlock(shape, childBlocks) => childBlocks
              case simpleBlock: SimpleBlock         => Set(simpleBlock)
            }
            lazy val block2Blocks = block2 match {
              case ComplexBlock(shape, childBlocks) => childBlocks
              case simpleBlock: SimpleBlock         => Set(simpleBlock)
            }

            if (
              smallest.bottomLeft.x == biggest.bottomLeft.x && biggest.bottomLeft.y == smallest.bottomLeft.y + smallest.height && smallest.width == biggest.width
            ) {
              val mergedBlock = ComplexBlock(
                Shape(smallest.bottomLeft, smallest.width, smallest.height + biggest.height),
                block1Blocks ++ block2Blocks
              )
              val nextBlockId = program.blockId + 1
              val nextBlocks =
                program.canvas.blocks.removed(blockId1).removed(blockId2).updated(nextBlockId.toString, mergedBlock)
              val nextCost = program.cost + costOf(move, program.canvas.shape, if (block1.shape.size > block2.shape.size) block1.shape else block2.shape)

              Right(
                program.copy(
                  canvas = program.canvas.copy(blocks = nextBlocks),
                  moves = nextMoves,
                  cost = nextCost,
                  blockId = nextBlockId
                )
              )
            } else if (
              smallest.bottomLeft.y == biggest.bottomLeft.y && biggest.bottomLeft.x == smallest.bottomLeft.x + smallest.width && smallest.height == biggest.height
            ) {
              val mergedBlock = ComplexBlock(
                Shape(smallest.bottomLeft, smallest.width + biggest.width, smallest.height),
                block1Blocks ++ block2Blocks
              )
              val nextBlockId = program.blockId + 1
              val nextBlocks =
                program.canvas.blocks.removed(blockId1).removed(blockId2).updated(nextBlockId.toString, mergedBlock)
              val nextCost = program.cost + costOf(move, program.canvas.shape, if (block1.shape.size > block2.shape.size) block1.shape else block2.shape)

              Right(
                program.copy(
                  canvas = program.canvas.copy(blocks = nextBlocks),
                  moves = nextMoves,
                  cost = nextCost,
                  blockId = nextBlockId
                )
              )
            } else
              Left(Errors.nonAdjointBlocks(blockId1, blockId2))
        }

      case SwapMove(blockId1, blockId2) =>
        (program.canvas.blocks.get(blockId1), program.canvas.blocks.get(blockId2)) match {
          case (None, None) =>
            Left(Errors.unknownBlockIds(blockId1, blockId2))

          case (None, _) =>
            Left(Errors.unknownBlockId(blockId1))

          case (_, None) =>
            Left(Errors.unknownBlockId(blockId2))

          case (Some(block1), Some(block2))
              if block1.shape.width != block2.shape.width || block1.shape.height != block2.shape.height =>
            Left(Errors.incompatibleShapes(blockId1, blockId2))

          case (Some(block1), Some(block2)) =>
            val nextBlocks = program.canvas.blocks
              .updated(blockId1, offset(block2, block1.shape.bottomLeft))
              .updated(blockId2, offset(block1, block2.shape.bottomLeft))
            // TODO: Check if shape is correct here.
            val nextCost = program.cost + costOf(move, program.canvas.shape, block1.shape)

            Right(
              program.copy(
                canvas = program.canvas.copy(blocks = nextBlocks),
                moves = nextMoves,
                cost = nextCost
              )
            )
        }
    }
  }

  def apply(program: Program, moves: List[Move]): Either[String, Program] = {
    var current: Either[String, Program] = Right(program)
    val vecMoves = moves.toVector
    var i = 0
    while (i < vecMoves.length && current.isRight) {
      current = apply(current.toOption.get, moves(i))
      i += 1
    }
    current
  }

  def paint(program: Program): BufferedImage = {
    val width = program.canvas.shape.width
    val height = program.canvas.shape.height

    val image =
      new BufferedImage(program.canvas.shape.width, program.canvas.shape.height, BufferedImage.TYPE_INT_ARGB)

    def paintShape(shape: Shape, color: Color): Unit =
      (shape.bottomLeft.x until (shape.bottomLeft.x + shape.width)).foreach(x =>
        (shape.bottomLeft.y until (shape.bottomLeft.y + shape.height)).foreach(y =>
          image.setRGB(x, height - y - 1, color.toInt)
        )
      )

    def paintBlock(block: Block): Unit =
      block match {
        case SimpleBlock(shape, color)    => paintShape(shape, color)
        case ComplexBlock(_, childBlocks) => childBlocks.foreach(paintBlock)
      }

    program.canvas.blocks.values.foreach(paintBlock)

    image
  }
}
