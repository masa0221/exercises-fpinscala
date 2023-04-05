val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "exercises-fpinscala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest-freespec" % "3.2.10" % Test,
      "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.10" % Test,
      )
  )
