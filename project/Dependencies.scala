import sbt._

object Dependencies {
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"
  lazy val specs2 = "org.specs2" %% "specs2-core" % "4.5.1"
  lazy val specs2Check = "org.specs2" %% "specs2-scalacheck" % "4.5.1"
}
