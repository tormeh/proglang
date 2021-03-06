
name := "solution"

organization := "de.tuberlin.uebb"

version := "0.1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq("org.specs2"  %% "specs2"  % "2.3.12"  withSources())
	//"org.specs2" %% "specs2" % "2.3.12" % "test")
    //"org.specs2" %% "specs2" % "1.12.4.1" % "test"
    // with Scala 2.9.3 (specs2 1.12.4.1 is the latest version for scala 2.9.3)


scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

unmanagedBase := baseDirectory.value / "lib"


