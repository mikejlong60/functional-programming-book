import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "functional-programming-book",
      scalaVersion := "2.13.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "functional-programming-book",
    libraryDependencies ++= List(scalaCheck)
  )
