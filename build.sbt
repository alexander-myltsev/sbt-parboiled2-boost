sbtPlugin := true

name := "sbt-parboiled2-boost"

organization := "name.myltsev"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.parboiled"                %% "parboiled"             % "2.0.1",
  "org.apache.commons"            % "commons-io"            % "1.3.2"
)
