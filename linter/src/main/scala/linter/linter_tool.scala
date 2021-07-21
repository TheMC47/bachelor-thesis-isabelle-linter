package linter

import isabelle._
import scala.collection.immutable

object Linter_Tool {

  def lint(
      options: Options,
      logic: String,
      progress: Progress = new Progress,
      log: Logger = No_Logger,
      dirs: List[Path] = Nil,
      select_dirs: List[Path] = Nil,
      selection: Sessions.Selection = Sessions.Selection.empty
  ) {

    // Use Dump to get a context...
    val context =
      Dump.Context(
        options,
        progress = progress,
        dirs = dirs,
        select_dirs = select_dirs,
        selection = selection,
        skip_base = true
      )

    val linter_variable = new Linter_Variable(Text_Reporter, cache = false)
    linter_variable.update(options + "linter=true")

    val linter = linter_variable.get.get

    context.build_logic(logic)
    context
      .sessions(logic, log = log)
      .foreach(_.process((args: Dump.Args) => {
        progress.echo("Processing theory " + args.print_node + " ...")
        val snapshot = args.snapshot
        val report = linter.report_for_snapshot(snapshot)
        progress.echo(report)
      }))

    context.check_errors
  }

  /* Isabelle tool wrapper */

  val isabelle_tool =
    Isabelle_Tool(
      "lint",
      "lint theory sources based on PIDE markup",
      Scala_Project.here,
      args => {
        var base_sessions: List[String] = Nil
        var select_dirs: List[Path] = Nil
        var requirements = false
        var exclude_session_groups: List[String] = Nil
        var all_sessions = false
        var dirs: List[Path] = Nil
        var session_groups: List[String] = Nil
        var logic = Dump.default_logic
        var options = Options.init()
        var verbose = false
        var exclude_sessions: List[String] = Nil

        val getopts = Getopts(
          """
Usage: isabelle lint [OPTIONS] [SESSIONS ...]

  Options are:
    -B NAME      include session NAME and all descendants
    -D DIR       include session directory and select its sessions
    -R           refer to requirements of selected sessions
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -b NAME      base logic image (default """ + isabelle.quote(Dump.default_logic) + """)
    -d DIR       include session directory
    -g NAME      select session group NAME
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -v           verbose
    -x NAME      exclude session NAME and all descendants

  Lint isabelle theories.
""",
          "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
          "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
          "R" -> (_ => requirements = true),
          "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
          "a" -> (_ => all_sessions = true),
          "b:" -> (arg => logic = arg),
          "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
          "g:" -> (arg => session_groups = session_groups ::: List(arg)),
          "o:" -> (arg => options = options + arg),
          "v" -> (_ => verbose = true),
          "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg))
        )

        val sessions = getopts(args)
        val progress = new Console_Progress(verbose = verbose)
        progress.interrupt_handler {
          lint(
            options,
            logic,
            progress = progress,
            dirs = dirs,
            select_dirs = select_dirs,
            selection = Sessions.Selection(
              requirements = requirements,
              all_sessions = all_sessions,
              base_sessions = base_sessions,
              exclude_session_groups = exclude_session_groups,
              exclude_sessions = exclude_sessions,
              session_groups = session_groups,
              sessions = sessions
            )
          )
        }
      }
    )
}
