name := "hcluster"

version := "0.1"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-spellchecker" % "3.6.2",
  //"com.rockymadden.stringmetric" %% "stringmetric-core" % "0.27.3",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.0.4",
  "org.slf4j" % "slf4j-log4j12" % "1.7.5" ,
  "org.scalatest" %% "scalatest" % "2.1.4" % "test"
)
