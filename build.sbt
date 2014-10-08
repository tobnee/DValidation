name := "dvalidation"

version := "1.0"

scalaVersion := "2.11.2"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

