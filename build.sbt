import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Goose Game",
    libraryDependencies += scalaCheck % Test,
    libraryDependencies += specs2 % Test,
    libraryDependencies += specs2Check % Test,
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
