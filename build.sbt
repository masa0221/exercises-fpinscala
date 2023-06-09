val scala3Version = "3.2.2"

scalacOptions ++= Seq(          // use ++= to add to existing options
  "-encoding", "utf8",          // if an option takes an arg, supply it on the same line
  "-feature",                   // then put the next option on a new line for easy editing
  "-language:implicitConversions",
  "-Werror",
)

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
