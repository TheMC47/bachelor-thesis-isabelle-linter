import scala.sys.process.Process

import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._
import sbt.io.IO
import sbtassembly.AssemblyPlugin
import sbtassembly.AssemblyPlugin.autoImport._

/** Plugin for sbt projects defining a tool as Isabelle component. */
object IsabelleToolPlugin extends AutoPlugin {
  override def requires: Plugins = AssemblyPlugin

  object autoImport {
    lazy val isabelleExecutable = settingKey[File]("Isabelle executable")
    lazy val isabelleCommand =
      settingKey[String]("isabelle command for run task")
    lazy val isabelleComponentAssembly =
      taskKey[File]("isabelle component assembly task")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      isabelleComponentAssembly := {
        // Assemble fat jar for isabelle tool
        val fatJarName = assembly.value.getName
        val toolClass = (mainClass in (Compile, run)).value

        // Write settings file
        val file = (target in Compile).value / "etc" / "settings"
        val contents = "classpath \"$COMPONENT/" + crossTarget.value.getName + "/" + fatJarName + "\"\n" +
          toolClass.map("isabelle_scala_service \"" + _ + "\"\n").getOrElse("")
        IO.write(file, contents)

        file
      },
      run := {
        isabelleComponentAssembly.value

        // Parse tool args
        val args = spaceDelimited("<arg>").parsed

        val resultCode = runIsabelle(
          streams.value.log,
          isabelleExecutable.value.getAbsolutePath,
          isabelleCommand.value +: args
        )

        if (resultCode != 0) {
          throw new IllegalStateException(
            "Running isabelle tool failed with exit code " + resultCode
          )
        }
      }
    )

  def runIsabelle(logger: Logger, executable: String, args: Seq[String]): Int = {
    logger.info("Running isabelle " + args.mkString(" "))
    val process = Process(executable, args).run(logger)
    process.exitValue()
  }
}
