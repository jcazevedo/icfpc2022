package icfpc2022

import java.awt.image.BufferedImage

import cats.syntax.functor._
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

  def fromJson(json: Json, initialImage: Option[BufferedImage]): Canvas = {
    val simpleCanvas = json.as[SimpleCanvas].toOption.get
    Canvas(
      Shape(Coords(0, 0), simpleCanvas.width, simpleCanvas.height),
      simpleCanvas.blocks.map {
        case SimpleBlockWithColor(id, bottomLeft, topRight, color) =>
          id -> SimpleBlock(Shape(bottomLeft, topRight.x - bottomLeft.x, topRight.y - bottomLeft.y), color)
        case SimpleBlockWithImage(id, bottomLeft, topRight, Coords(pngX, pngY)) => {
          val shape = Shape(bottomLeft, topRight.x - bottomLeft.x, topRight.y - bottomLeft.y)
          id -> ComplexBlock(
            shape,
            (bottomLeft.x until topRight.x).flatMap { x =>
              (bottomLeft.y until topRight.y).map { y =>
                SimpleBlock(
                  Shape(Coords(x, y), 1, 1),
                  Color.fromInt(initialImage.get.getRGB(pngX + x, initialImage.get.getHeight() - (pngY + y) - 1))
                )
              }
            }.toSet
          )
        }
      }.toMap
    )
  }

  sealed trait SimpleBlockWithId {
    def blockId: String
    def bottomLeft: Coords
    def topRight: Coords
  }

  object SimpleBlockWithId {
    implicit val simpleBlockWithIdEncoder: Encoder[SimpleBlockWithId] = Encoder.instance {
      case x: SimpleBlockWithColor => Encoder[SimpleBlockWithColor].apply(x)
      case x: SimpleBlockWithImage => Encoder[SimpleBlockWithImage].apply(x)
    }
    implicit val simpleBlockWithIdDecoder: Decoder[SimpleBlockWithId] =
      Decoder[SimpleBlockWithColor].widen or Decoder[SimpleBlockWithImage].widen
  }

  case class SimpleBlockWithColor(blockId: String, bottomLeft: Coords, topRight: Coords, color: Color)
      extends SimpleBlockWithId

  object SimpleBlockWithColor {
    implicit val simpleBlockWithColorEncoder: Encoder[SimpleBlockWithColor] = deriveEncoder
    implicit val simpleBlockWithColorDecoder: Decoder[SimpleBlockWithColor] = deriveDecoder
  }

  case class SimpleBlockWithImage(blockId: String, bottomLeft: Coords, topRight: Coords, pngBottomLeftPoint: Coords)
      extends SimpleBlockWithId

  object SimpleBlockWithImage {
    implicit val simpleBlockWithImageEncoder: Encoder[SimpleBlockWithImage] = deriveEncoder
    implicit val simpleBlockWithImageDecoder: Decoder[SimpleBlockWithImage] = deriveDecoder
  }

  case class SimpleCanvas(width: Int, height: Int, blocks: List[SimpleBlockWithId])

  object SimpleCanvas {
    implicit val simpleCanvasEncoder: Encoder[SimpleCanvas] = deriveEncoder
    implicit val simpleCanvasDecoder: Decoder[SimpleCanvas] = deriveDecoder
  }
}
