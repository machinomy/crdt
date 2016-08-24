import sbtrelease.ReleasePlugin.autoImport.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._

name := "crdt"

version := "0.0.1"

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

def doIfNotSnapshot(step: ReleaseStep) = {
  ReleaseStep(
    action = st => {
      if (!st.get(versions).getOrElse((None, None))._1.toString.endsWith("-SNAPSHOT")) {
        step.action(st)
      } else {
        st
      }
    },
    check = step.check,
    enableCrossBuild = step.enableCrossBuild
  )
}

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  doIfNotSnapshot(setReleaseVersion),
  doIfNotSnapshot(commitReleaseVersion),
  doIfNotSnapshot(tagRelease),
  publishArtifacts,
  doIfNotSnapshot(setNextVersion),
  doIfNotSnapshot(commitNextVersion),
  doIfNotSnapshot(pushChanges)
)

publishTo := {
  if (isSnapshot.value)
    Some("Machinomy" at "http://artifactory.machinomy.com/artifactory/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  else
    Some("Machinomy" at "http://artifactory.machinomy.com/artifactory/libs-release-local/")
}
credentials += Credentials(new File("credentials.properties"))
