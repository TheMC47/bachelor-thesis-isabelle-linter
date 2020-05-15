
name := "sbt-isabelle-component"

version := "0.1"

scalaVersion := "2.12.10"

// Root project, aggregates sub-projects
lazy val root = (project in file("."))
  .settings(publish / skip := true)
  .aggregate(isabelle, `my-component`)

// Isabelle project dependency
lazy val isabelle = project
  .settings(
    publish / skip := true,
    unmanagedJars in Compile ++= (baseDirectory.value / "lib" / "classes" ** "*.jar").get(),
  )

// Our component!
lazy val `my-component` = project
  .settings(
    isabelleCommand := "my_tool_cmd",
    isabelleExecutable := (baseDirectory in isabelle).value / "bin" / "isabelle",
  )
  .enablePlugins(IsabelleToolPlugin)
  .dependsOn(isabelle)
