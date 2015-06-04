
name := "solution"

organization := "NTNU ITK"

version := "0.1.0"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-feature", "-optimise", "-Xlint", "-Xfatal-warnings", "-deprecation", "-Ywarn-unused", "-Ywarn-infer-any", "-Ywarn-unused-import", "-Ywarn-dead-code", "-Ywarn-inaccessible", "-Ywarn-numeric-widen", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-adapted-args")

libraryDependencies ++= Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3")


