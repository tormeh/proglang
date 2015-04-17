
name := "solution"

organization := "NTNU ITK"

version := "0.1.0"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-feature", "-optimize", "-Xlint", "-Xfatal-warnings")

libraryDependencies ++= Seq("org.specs2"  %% "specs2"  % "2.3.12"  withSources(), "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3")
	//"org.specs2" %% "specs2" % "2.3.12" % "test")
    //"org.specs2" %% "specs2" % "1.12.4.1" % "test"
    // with Scala 2.9.3 (specs2 1.12.4.1 is the latest version for scala 2.9.3)


scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

unmanagedBase := baseDirectory.value / "lib"


