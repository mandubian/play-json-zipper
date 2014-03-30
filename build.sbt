organization := "com.mandubian"

name    := "play-json-zipper"

version := "1.1"

resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
  "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"
)

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.typesafe.play"   %% "play-json"  % "2.2.0"          ,
  "org.specs2"          %% "specs2"     % "1.13"   % "test",
  "junit"                % "junit"      % "4.8"    % "test"
)

publishMavenStyle := true

seq(bintraySettings:_*)

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

bintray.Keys.packageLabels in bintray.Keys.bintray :=
  Seq("play-json", "zipper", "monad", "functional programming", "scala")
