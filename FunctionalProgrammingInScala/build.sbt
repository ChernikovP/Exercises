name := "FunctionalProgrammingInScala"

version := "1.0"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.0",
  "org.specs2" %% "specs2-core" % "4.3.4",
  "org.specs2" %% "specs2-scalacheck" % "4.3.4"
).map(_ % Test)
