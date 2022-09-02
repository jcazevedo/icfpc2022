import Dependencies._

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version      := "1.0.0"

// Enable the OrganizeImports Scalafix rule.
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

lazy val root = (project in file("."))
  .settings(
    name              := "icfpc2022",
    libraryDependencies ++= Seq(akkaActor, akkaHttp, scalaTest % Test),
    scalafmtOnCompile := true,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixOnCompile := true,
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-unchecked",
      "-feature",
      "-deprecation",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-macros:after",
      "-Xfatal-warnings"
    )
  )
