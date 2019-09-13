import Dependencies._

lazy val root = (project in file(".")).
  //enablePlugins(ScalaNativePlugin).  // If you want to compile natively you have to switch scala version to 2.11.12 until I figure stuff out. And there is also a scalatest error.  So you can only build the native executable, not run the tests.
  settings(
    inThisBuild(List(
      organization := "functional-programming-book",
      scalaVersion := "2.12.9",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "functional-programming-book",
    libraryDependencies ++= List(scalaTest, scalaCheck)
  )
