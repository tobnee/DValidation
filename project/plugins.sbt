logLevel := Level.Warn

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.2.1")

resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "0.99.10.2")

addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "0.99.0")