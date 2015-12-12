name := "adventofcode"
version := "1.0"
scalaVersion := "2.11.1"
mainClass in (Compile, run) := Some("main.scala.Day12")

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.3.4"
)

resolvers ++= Seq("Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/")
