ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "priceBasket2"
  )
libraryDependencies += "junit" % "junit" % "4.13.2" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test