
name := "sbt-isabelle-component"

version := "1.0"

scalaVersion in ThisBuild := "2.13.4"

// Root project, aggregates sub-projects
lazy val root = (project in file("."))
  .settings(publish / skip := true)
  .aggregate(pure, `my-component`)

// Isabelle/Pure
lazy val pure = (project in file("isabelle"))
  .settings(
    publish / skip := true,
    compile / skip := true,
    unmanagedJars in Compile ++= Seq(
      baseDirectory.value / "lib" / "classes" / "Pure.jar",
    ),
    libraryDependencies ++= Seq(
      "org.tukaani" % "xz" % "1.8"
    ),
    target := baseDirectory.value / "target" / "pure",
    scalaSource in Compile := baseDirectory.value / "src" / "Pure",
  )

// Executable
isabelleExecutable in ThisBuild := baseDirectory.value / "isabelle.sh"


// Our component!
lazy val `my-component` = project
  .settings(
    isabelleCommand := "my_tool_cmd",
  )
  .enablePlugins(IsabelleToolPlugin)
  .dependsOn(pure)