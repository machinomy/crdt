name := "crdt"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.6.1",
  "com.github.nscala-time" %% "nscala-time" % "2.10.0",
  "org.scala-graph" %% "graph-core" % "1.11.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.1" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.9" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.4.9" % "test"
)
