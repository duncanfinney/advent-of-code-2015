name := "adventofcode"
version := "1.0"
scalaVersion := "2.11.1"
mainClass in (Compile, run) := Some("main.scala.Day12")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.4",
  "com.lihaoyi" %% "fastparse" % "0.3.4",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)

resolvers ++= Seq("Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/")
