import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest"     %% "scalatest"  % "3.2.11"
  lazy val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.6.19"
  lazy val akkaHttp  = "com.typesafe.akka" %% "akka-http"  % "10.2.9"
}
