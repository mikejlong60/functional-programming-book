import Dependencies._
//import sbt.Keys.scalacOptions

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "functional-programming-book",
      scalaVersion := "2.12.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "functional-programming-book",
    libraryDependencies ++= List(scalaTest, scalaCheck)
  )
