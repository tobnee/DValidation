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

lazy val jdk8time = dvalProject("validators-jdk8time")
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion % "provided"
  )
  .dependsOn(core % "compile->compile;test->test")

scoverage.ScoverageSbtPlugin.instrumentSettings

org.scoverage.coveralls.CoverallsPlugin.coverallsSettings
