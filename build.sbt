name := "sbt-isabelle-linter"

version := "1.1"

scalaVersion in ThisBuild := "2.13.4"

// Root project, aggregates sub-projects
lazy val root = (project in file("."))
  .settings(publish / skip := true)
  .aggregate(`linter`)

// Isabelle/Pure
lazy val isabelle = project.enablePlugins(IsabellePlugin)

// Our component!
lazy val `linter` = project
  .settings(
    isabelleProject := isabelle,
    isabelleCommand := "lint",
    crossTarget := baseDirectory.value / "../isabelle/src/Tools/jEdit/target/linter",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  )
  .enablePlugins(IsabelleToolPlugin) // Tool wrapper and run
  .dependsOn(isabelle) // Compilation
