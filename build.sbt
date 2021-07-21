// data-science-from-scratch-scala

lazy val root = (project in file(".")).
  settings(
    name         := "data-science-from-scratch-scala",
    version      := "0.1.0",
    scalaVersion := "2.13.5")

libraryDependencies += "me.shadaj" %% "scalapy-core" % "0.5.0" // to use python from Scala somewhat seamlessly

