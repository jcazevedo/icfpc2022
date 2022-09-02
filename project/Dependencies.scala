import sbt._

object Dependencies {
  lazy val akkaActor  = "com.typesafe.akka" %% "akka-actor"  % "2.6.19"
  lazy val akkaHttp   = "com.typesafe.akka" %% "akka-http"   % "10.2.9"
  lazy val akkaStream = "com.typesafe.akka" %% "akka-stream" % "2.6.19"
  lazy val scalaTest  = "org.scalatest"     %% "scalatest"   % "3.2.11"
}
