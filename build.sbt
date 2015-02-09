import Build._

lazy val core = dvalProject("core")
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion % "provided",
    libraryDependencies += "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
  )

lazy val playJson = dvalProject("play-json")
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion % "provided",
    libraryDependencies +=  "com.typesafe.play" %% "play-json" % "2.3.5"
  )
  .dependsOn(core)

scoverage.ScoverageSbtPlugin.instrumentSettings

org.scoverage.coveralls.CoverallsPlugin.coverallsSettings
