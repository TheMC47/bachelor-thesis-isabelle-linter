
name := "sbt-isabelle-component"

version := "1.1"

scalaVersion in ThisBuild := "2.13.4"

// Root project, aggregates sub-projects
lazy val root = (project in file("."))
  .settings(publish / skip := true)
  .aggregate(`my-component`)

// Isabelle/Pure
lazy val isabelle = project.enablePlugins(IsabellePlugin)

// Our component!
lazy val `my-component` = project
  .settings(
    isabelleProject := isabelle,
    isabelleCommand := "my_tool_cmd",
  )
  .enablePlugins(IsabelleToolPlugin) // Tool wrapper and run
  .dependsOn(isabelle) // Compilation