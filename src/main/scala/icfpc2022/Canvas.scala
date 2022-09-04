package icfpc2022

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json}

case class Canvas(shape: Shape, blocks: Map[String, Block]) {
  lazy val blockSet: Set[Block] =
    blocks.values.toSet

  lazy val simpleBlockSet: Set[SimpleBlock] =
    blocks.values.flatMap {
      case block: SimpleBlock      => Set(block)
      case ComplexBlock(_, blocks) => blocks
    }.toSet

  lazy val coordsSet: Set[Coords] =
    blocks.values.flatMap {
      case SimpleBlock(shape, _)   => Set(shape.bottomLeft)
      case ComplexBlock(_, blocks) => blocks.map(_.shape.bottomLeft).toSet
    }.toSet

  def colorOf(x: Int, y: Int) =
    simpleBlockSet
      .find { case SimpleBlock(shape, _) =>
        x >= shape.bottomLeft.x && y >= shape.bottomLeft.y && x < shape.topRight.x && y < shape.topRight.y
      }
      .get
      .color
}

object Canvas {
  def blank(width: Int, height: Int): Canvas = {
    val fullShape = Shape(Coords(0, 0), width, height)
    Canvas(fullShape, Map("0" -> SimpleBlock(fullShape, Color(255, 255, 255, 255))))
  }

  def fromJson(json: Json): Canvas = {
    val simpleCanvas = json.as[SimpleCanvas].toOption.get
    Canvas(
      Shape(Coords(0, 0), simpleCanvas.width, simpleCanvas.height),
      simpleCanvas.blocks.map { case SimpleBlockWithId(id, bottomLeft, topRight, color) =>
        id -> SimpleBlock(Shape(bottomLeft, topRight.x - bottomLeft.x, topRight.y - bottomLeft.y), color)
      }.toMap
    )
  }

  case class SimpleBlockWithId(blockId: String, bottomLeft: Coords, topRight: Coords, color: Color)

  object SimpleBlockWithId {
    implicit val simpleBlockWithIdEncoder: Encoder[SimpleBlockWithId] = deriveEncoder
    implicit val simpleBlockWithIdDecoder: Decoder[SimpleBlockWithId] = deriveDecoder
  }

  case class SimpleCanvas(width: Int, height: Int, blocks: List[SimpleBlockWithId])

  object SimpleCanvas {
    implicit val simpleCanvasEncoder: Encoder[SimpleCanvas] = deriveEncoder
    implicit val simpleCanvasDecoder: Decoder[SimpleCanvas] = deriveDecoder
  }
}
