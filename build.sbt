name := "dvalidation"

organization := "net.atinu"

version := "7.2.7.0.1"

scalaVersion := "2.11.2"

scalacOptions  ++= Seq("-unchecked", "-deprecation", "-feature")

crossScalaVersions := Seq("2.11.2", "2.10.4")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

val scalazVersion = "7.2.7"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion % "provided"

libraryDependencies += "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

scoverage.ScoverageSbtPlugin.instrumentSettings

org.scoverage.coveralls.CoverallsPlugin.coverallsSettings

scalariformSettings

publishMavenStyle := true


