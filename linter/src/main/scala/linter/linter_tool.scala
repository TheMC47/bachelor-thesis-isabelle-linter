package linter

import isabelle._
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object Linter_Tool {

  abstract class Lint_CLI[A] {

    def get_linter_variable: Linter_Variable[A]

    def process_args(
        linter: Linter_Interface[A],
        args: Dump.Args,
        progress: Progress
    ): Unit = ()

    def process_end(
        progress: Progress
    ): Unit = ()

    def apply(
        options: Options,
        logic: String,
        progress: Progress = new Progress,
        log: Logger = No_Logger,
        dirs: List[Path] = Nil,
        select_dirs: List[Path] = Nil,
        selection: Sessions.Selection = Sessions.Selection.empty
    ): Unit = {
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

      val linter_variable = get_linter_variable
      linter_variable.update(options + "linter=true")

      val linter = linter_variable.get.get

      context.build_logic(logic)
      context
        .sessions(logic, log = log)
        .foreach(_.process((args: Dump.Args) => {
          progress.echo("Processing theory " + args.print_node + " ...")
          process_args(linter, args, progress)
        }))
      context.check_errors
      process_end(progress)
    }
  }

  class Lint_JSON extends Lint_CLI[JSON.T] {

    val reports = new ListBuffer[JSON.T]()

    def get_linter_variable: Linter_Variable[JSON.T] =
      new Linter_Variable(JSON_Reporter, cache = false)

    override def process_args(
        linter: Linter_Interface[JSON.T],
        args: Dump.Args,
        progress: Progress
    ): Unit = {
      val start_date = Date.now()
      val report = linter.report_for_snapshot(args.snapshot)
      val end_date = Date.now()
      val timing = end_date.time - start_date.time
      reports += JSON.Object(
        "theory" -> args.print_node,
        "report" -> report,
        "timing" -> timing.ms
      )
    }

    override def process_end(
        progress: Progress
    ): Unit = {
      progress.echo(
        JSON.Format(
          JSON.Object(
            "reports" -> reports.toList
          )
        )
      )
    }
  }

  object Lint_Text extends Lint_CLI[String] {
    override def get_linter_variable: Linter_Variable[String] =
      new Linter_Variable(Text_Reporter, cache = false)

    override def process_args(
        linter: Linter_Interface[String],
        args: Dump.Args,
        progress: Progress
    ): Unit =
      progress.echo(linter.report_for_snapshot(args.snapshot))

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
        var mode: String = "text"

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
    -r MODE      how to report results (either "text" or "json", default "text")

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
          "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)),
          "r:" -> (arg => mode = arg)
        )

        val sessions = getopts(args)

        val lint = mode match {
          case "text" => Lint_Text
          case "json" => new Lint_JSON()
          case _ => error(s"Unrecognized reporting mode $mode")
        }

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
