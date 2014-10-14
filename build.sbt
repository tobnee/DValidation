name := "dvalidation"

organization := "net.atinu"

version := "0.1"

scalaVersion := "2.11.2"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

scalariformSettings

