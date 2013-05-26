import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "multiservices"
  val appVersion      = "1.0-SNAPSHOT"

  val mandubianRepo = Seq(
    "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"
  )

  val appDependencies = Seq()

  val main = play.Project(appName, appVersion, appDependencies).settings(
    resolvers ++= mandubianRepo,
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "play2-reactivemongo" % "0.9",
      "org.reactivemongo" %% "reactivemongo"       % "0.9",
      "play"              %% "play-json"           % "2.2-SNAPSHOT",
      "play-json-zipper"  %% "play-json-zipper"    % "0.1-SNAPSHOT",
      "org.specs2"        %% "specs2"              % "1.13"        % "test",
      "junit"              % "junit"               % "4.8"         % "test"
    )
  )

}
