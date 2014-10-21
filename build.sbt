name := "dvalidation"

organization := "net.atinu"

version := "0.1"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.11.2", "2.10.4")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

val scalazVersion = "7.0.6"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion

libraryDependencies += "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

scalariformSettings

