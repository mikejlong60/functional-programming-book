import Dependencies._
//import sbt.Keys.scalacOptions

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "functional-programming-book",
      scalaVersion := "2.12.9",//"2.11.12", //"2.12.9",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "functional-programming-book",
    libraryDependencies ++= List(scalaTest, scalaCheck)
  )
