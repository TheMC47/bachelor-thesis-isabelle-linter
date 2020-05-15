package mwe

import isabelle._

/* This is the component. */
object My_Component
{

  /* This is the command-line wrapper */
  val isabelle_tool = Isabelle_Tool("my_tool_cmd", "Run my tool", args =>
  {
    val getopts = Getopts("""
Usage: isabelle my_tool_cmd

  Options are:

  Does nothing (yet).
""")
    /* some code */

    val progress = new Console_Progress()
    progress.echo("Hello world")
  })
}