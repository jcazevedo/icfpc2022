package icfpc2022

import java.io.{File, PrintWriter}
import javax.imageio.ImageIO

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, Multipart, Uri}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import icfpc2022.syntax._

object Main extends App {
  implicit val system = ActorSystem()
  implicit val executionContext = system.dispatcher

  val config = ConfigFactory.load()

  val API_KEY = config.getString("api-key")
  val SUBMIT = config.getBoolean("submit")
  val URL = config.getString("url")

  (1 to 25).foreach { id =>
    val problem = s"problems/$id.png"
    val islFile = s"isls/$id.isl"
    val outputFile = s"output/$id.png"

    println(s"Starting to solve problem $problem...")

    val (p, cost) = RecursiveDivisionSolver.solve(new File(problem))
    val isl = p.isl

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
