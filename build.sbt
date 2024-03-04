lazy val root = (project in file("."))
  .settings(
    name            := "requirements-as-types",
    organization    := "Michał Płachta",
    version         := "1.0",
    scalaVersion    := "3.3.3",
    scalacOptions ++= List("-unchecked", "-deprecation", "-explain"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.3",
      "org.scalatest" %% "scalatest"   % "3.2.18" % Test
    ),
    initialCommands := s"""
      import cats.effect._, cats.implicits._, cats.effect.unsafe.implicits.global
      import scala.concurrent.duration._, java.util.concurrent._
    """,
    run / fork      := true,
    run / javaOptions += "-ea"
  )
