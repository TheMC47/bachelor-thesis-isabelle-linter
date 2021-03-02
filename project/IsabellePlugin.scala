import scala.sys.process.Process

import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._

/**
 * Isabelle plugin wrapper. Run the repo Isabelle Instance only using this wrapper.
 */
object IsabellePlugin extends AutoPlugin {
  val USER_HOME = "USER_HOME"

  private def runIsabelle(bin: File, root: File, cmd: Seq[String], home: File, logger: Logger): Unit = {
    val process = Process(bin.getAbsolutePath +: cmd, root, USER_HOME -> home.getAbsolutePath).run(logger)
    val resultCode = process.exitValue()
    if (resultCode != 0) {
      throw new IllegalStateException(s"Running Isabelle command failed with exit code $resultCode")
    }
  }

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      publish / skip := true,
      compile / skip := true,
      unmanagedJars in Compile ++= (baseDirectory.value / "lib" / "classes" ** "*.jar").get(),
      libraryDependencies += "org.tukaani" % "xz" % "1.8",
      scalaSource in Compile := baseDirectory.value / "src",
      run := {
        val isabelleExecutable = baseDirectory.value / "bin" / "isabelle"
        val projectDir = baseDirectory.value / ".."

        // Run isabelle process
        val logger = streams.value.log

        // Prepare Isabelle instance if necessary
        runIsabelle(isabelleExecutable, projectDir, Seq("components", "-a"), projectDir, logger)

        // Parse user invocation
        val args = spaceDelimited("<arg>").parsed
        logger.info("Running isabelle " + args.mkString(" "))

        runIsabelle(isabelleExecutable, projectDir, args, projectDir, logger)
      }
    )
}
