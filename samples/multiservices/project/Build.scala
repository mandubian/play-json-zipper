import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "multiservices"
  val appVersion      = "1.0-SNAPSHOT"

  val mandubianRepo = Seq(
    "mandubian maven bintray" at "http://dl.bintray.com/mandubian/maven"
    //"Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
    //"Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"
  )

  val sonatypeRepo = Seq( 
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
  )

  val appDependencies = Seq()

  val main = play.Project(appName, appVersion, appDependencies).settings(
    resolvers ++= mandubianRepo ++ sonatypeRepo,
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "play2-reactivemongo" % "0.10.0",
      "com.typesafe.play" %% "play-json"           % "2.2.0",
      "com.mandubian"     %% "play-json-zipper"    % "1.1",
      "org.specs2"        %% "specs2"              % "1.13"        % "test",
      "junit"              % "junit"               % "4.8"         % "test"
    )
  )

}
