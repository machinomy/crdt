import sbtrelease.ReleaseStateTransformations._
import de.heikoseeberger.sbtheader.license.Apache2_0

name := "crdt"

version := "0.0.2"

scalaVersion := "2.11.8"

organization := "com.machinomy"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.6.1",
  "com.github.nscala-time" %% "nscala-time" % "2.10.0",
  "org.scala-graph" %% "graph-core" % "1.11.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.1" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.9" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.4.9" % "test"
)

def whenRelease(releaseStep: ReleaseStep): ReleaseStep =
  releaseStep.copy(state => if (Project.extract(state).get(isSnapshot)) state else releaseStep.action(state))

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  runClean,
  runTest,
  whenRelease(tagRelease),
  publishArtifacts,
  whenRelease(pushChanges)
)

publishTo := {
  val base = "http://artifactory.machinomy.com/artifactory"
  if (isSnapshot.value) {
    val timestamp = new java.util.Date().getTime
    Some("Machinomy" at s"$base/snapshot;build.timestamp=$timestamp")
  } else {
    Some("Machinomy" at s"$base/release")
  }
}

credentials += Credentials(new File("credentials.properties"))

licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

headers := Map(
  "scala" -> Apache2_0("2016", "Machinomy")
)

autoAPIMappings := true

scalacOptions := Seq("-feature")

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")
