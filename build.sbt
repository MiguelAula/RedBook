name := "training"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq("-Wconf:cat=deprecation:ws,any:e")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
