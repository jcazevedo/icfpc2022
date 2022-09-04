package icfpc2022

import java.io.{File, PrintWriter}
import javax.imageio.ImageIO

import scala.concurrent.Await
import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, Multipart}
import com.typesafe.config.ConfigFactory
import icfpc2022.syntax._
import io.circe.parser

object Main extends App {
  implicit val system = ActorSystem()
  implicit val executionContext = system.dispatcher

  val config = ConfigFactory.load()

  val API_KEY = config.getString("api-key")
  val SUBMIT = config.getBoolean("submit")
  val URL = config.getString("url")

  (36 to 36).foreach { id =>
    val problem = s"problems/$id.png"
    val initialCanvasJson = s"problems/$id.initial.json"
    val initialCanvasPNG = s"problems/$id.initial.png"
    val islFile = s"isls/$id.isl"
    val outputFile = s"output/$id.png"

    println(s"Starting to solve problem $problem...")

    val image = ImageIO.read(new File(problem))
    val initialCanvas = {
      val initialCanvasFile = new File(initialCanvasJson)
      val initialCanvasPNGFile = new File(initialCanvasPNG)
      if (initialCanvasFile.exists()) {
        val source = scala.io.Source.fromFile(initialCanvasFile)
        val content = source.getLines().mkString
        source.close()
        val json = parser.parse(content).toOption.get
        val initialImage =
          if (initialCanvasPNGFile.exists()) Some(ImageIO.read(initialCanvasPNGFile))
          else None
        Canvas.fromJson(json, initialImage)
      } else {
        Canvas.blank(image.getWidth(), image.getHeight())
      }
    }
    val p = Solver.solve(image, initialCanvas)
    val isl = p.isl
    val cost = Scorer.score(p, image)

    println(s"Solved problem $problem with a cost of $cost")

    println(s"Writing ISL to $islFile")
    val writer = new PrintWriter(islFile)
    writer.write(isl)
    writer.close()

    println(s"Writing image to $outputFile")
    ImageIO.write(Interpreter.paint(p), "png", new File(outputFile))

    if (SUBMIT) {
      val request =
        HttpRequest(
          method = HttpMethods.POST,
          uri = s"$URL/api/submissions/$id/create",
          entity = Multipart
            .FormData(
              Multipart.FormData.BodyPart
                .Strict("file", HttpEntity(ContentTypes.`text/plain(UTF-8)`, isl), Map("filename" -> "submission.isl"))
            )
            .toEntity,
          headers = Seq(Authorization(OAuth2BearerToken(API_KEY)))
        )

      println("Submitting solution...")
      val result = Await.result(Http().singleRequest(request), Duration.Inf)
      val responseBody = Await.result(result.entity.toStrict(1.minute), Duration.Inf)
      println(s"Response: ${responseBody.data.utf8String}")
    }
    println()
  }

  system.terminate()
}
