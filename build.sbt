import Dependencies._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "day-01"
ThisBuild / organization     := "io.databrewers"
ThisBuild / organizationName := "databrewers"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
