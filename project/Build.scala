import sbt._
import Keys._

object ApplicationBuild extends Build {
  val buildName         = "play-json-zipper"

  val buildVersion      = "1.0"

  val mandubianRepo = Seq(
    "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"
  )

  lazy val playJsZipper = Project(
    buildName, file("."),
    settings = Defaults.defaultSettings ++ Seq(
      resolvers ++= mandubianRepo,
      scalaVersion := "2.10.2",
      version      := buildVersion,
      libraryDependencies ++= Seq(
        "com.typesafe.play"   %% "play-json"  % "2.2.0"          ,
        "org.specs2"          %% "specs2"     % "1.13"   % "test",
        "junit"                % "junit"      % "4.8"    % "test"
      ),
      publishMavenStyle := true,
      publishTo <<= version { (version: String) =>
        val localPublishRepo = "../mandubian-mvn/"
        if(version.trim.endsWith("SNAPSHOT"))
          Some(Resolver.file("snapshots", new File(localPublishRepo + "/snapshots")))
        else Some(Resolver.file("releases", new File(localPublishRepo + "/releases")))
      }
    )
  )
}
