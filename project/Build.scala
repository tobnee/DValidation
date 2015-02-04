import com.typesafe.sbt.SbtScalariform._
import sbt.Keys._
import sbt._

object Build {

  def RepoSettings = List(
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomExtra := (
      <url>https://github.com/tobnee/DValidation</url>
        <licenses>
          <license>
            <name>Apache 2 license</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:tobnee/DValidation.git</url>
          <connection>scm:git:git@github.com:tobnee/DValidation.git</connection>
        </scm>
        <developers>
          <developer>
            <id>tobnee</id>
            <name>Tobias Neef</name>
            <url>http://atinu.net/</url>
          </developer>
        </developers>)
  )

  val scalazVersion = "7.1.0"

  def dvalProject(id: String): Project = Project(id, file("dvalidation-" + id))
    .settings(
      name := s"dvalidation-$id",
      organization := "net.atinu",
      version := "0.3-SNAPSHOT",
      scalaVersion := "2.11.2",
      scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
      crossScalaVersions := Seq("2.11.2", "2.10.4"),
      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases")
      )
    ).settings(RepoSettings: _*)
     .settings(scalariformSettings: _*)

}