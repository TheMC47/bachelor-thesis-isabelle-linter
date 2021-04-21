package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable
import scala.util.parsing.combinator._
import scala.util.parsing.input

object Linter {

  def lint(
      options: Options,
      session_name: String,
      theories: List[String],
      logic: String,
      progress: Progress = new Progress,
      log: Logger = No_Logger,
      dirs: List[Path] = Nil,
      select_dirs: List[Path] = Nil,
      verbose: Boolean = false
  ) {

    // Build the session
    progress.echo("##### BUILDING ######")

    Build.build(
      options,
      progress = progress,
      select_dirs = select_dirs,
      verbose = verbose
    )

    progress.echo("##### Built! ######")

    // Read the session to get the snapshot
    progress.echo("##### Reading ######")

    progress.echo(s"Session name $session_name")

    val store = Sessions.store(options)
    val resources = Resources.empty
    val session = new Session(options, resources)

    using(store.open_database_context())(db_context => {
      val result =
        db_context.input_database(session_name)((db, _) => {
          val theories = store.read_theories(db, session_name)
          val errors = store.read_errors(db, session_name)
          store.read_build(db, session_name).map(info => (theories, errors, info.return_code))
        })
      result match {
        case None => error("Missing build database for session " + quote(session_name))
        case Some((used_theories, errors, rc)) =>
          val bad_theories = theories.filterNot(used_theories.toSet)
          if (bad_theories.nonEmpty) error("Unknown theories " + commas_quote(bad_theories))

          val print_theories =
            if (theories.isEmpty) used_theories else used_theories.filter(theories.toSet)

          for (thy <- print_theories) {
            val thy_heading = "\nTheory " + quote(thy) + ":"
            Build_Job.read_theory(db_context, resources, session_name, thy) match {
              case None => progress.echo(thy_heading + " MISSING")
              case Some(command) =>
                progress.echo(thy_heading + "FOUND")
                val snapshot = Document.State.init.snippet(command)
                val node = snapshot.node
                val commands = node.commands
                commands.iterator foreach (debug_command(_, progress, snapshot))
            }
          }
          if (errors.nonEmpty) {
            progress.echo("\nBuild errors:\n" + cat_lines(errors))
          }
          if (rc != 0) progress.echo("\n" + Process_Result.print_return_code(rc))
      }
    })

  }

  def debug_command(command: Command, progress: Progress, snapshot: Document.Snapshot) = {

    // Print stuff that you think is useful
    def markups(state: Command.State): List[Markup] = state.status
    val span = command.span
    progress.echo(command.source)
    progress.echo("----------")
    progress.echo("name: " + span.name)
    progress.echo("kind: " + span.kind.toString)
    progress.echo("position: " + span.position.toString)
    val tokens: List[Token] = span.content filterNot (_.is_space)
    val s: List[String] = tokens.map(t => t.kind.toString + "-" + t.source)
    progress.echo(s.mkString("Tokens_sources(", ",", ")"))
    val cmd_states = snapshot.state.command_states(snapshot.version, command)
    progress.echo("Number of command states: " + cmd_states.length.toString())
    val markups_str = markups(cmd_states.head).map(_.name).mkString(",")
    progress.echo("Markups (for first cmd state): " + markups_str)
    val parseResult = TokenParsers.parse(TokenParsers.tokenParser, tokens) match {
      case TokenParsers.Success(result, next) =>
        s"Success. Parsed: $result, left: $next"
      case _ => "Failed"
    }
    progress.echo(s"Parse Result: $parseResult")
    progress.echo("##########")

  }

  /* ==== Parsing ====
   * Try to map token streams into something that has more structure.
   * */

  case class TokenReader(in: List[Token]) extends input.Reader[Token] {
    def first: Token = in.head
    def rest: TokenReader = TokenReader(in.tail)
    def pos: input.Position = input.NoPosition
    def atEnd: Boolean = in.isEmpty
    def apply(in: List[Token]): TokenReader =
      new TokenReader(in filterNot (_.is_space))
  }

  abstract class DocumentElement
  abstract class Proof extends DocumentElement
  case class Sorry() extends Proof
  case class Apply() extends Proof
  case class Unparsed() extends DocumentElement

  object TokenParsers extends Parsers {
    type Elem = Token

    def pBool(p: Token => Boolean): Parser[Token] = new Parser[Token] {
      def apply(in: Input): ParseResult[Token] = {
        if (in.atEnd) Failure("Nothing to parse", in)
        else {
          val token = in.first
          if (p(in.first)) Success(in.first, in.rest)
          else Failure("failed", in)
        }
      }
    }

    def pSorry: Parser[Sorry] = pBool(_.is_command("sorry")) ~> success(Sorry())

    def tokenParser: Parser[DocumentElement] = pSorry

    def parse[T](p: Parser[T], in: List[Token]): ParseResult[T] =
      p(TokenReader(in))
  }

  /* Structures for parsing */
  // For now, just the index of the xml node
  case class Span(startLine: Int, endLine: Int) {
    def report(progress: Progress, s: String): Unit = progress.echo(s"At line $startLine: $s")
  }

  // Just a placeholder. This should look for a configuration file and parse it,
  // read command line args…
  // def getLints(): List[Lint] = List(NoSorry)

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
        var theories: List[String] = Nil

        val getopts = Getopts(
          """
Usage: isabelle lint [OPTIONS] SESSION

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
    -u OPT       overide update option: shortcut for "-o update_OPT"
    -v           verbose
    -x NAME      exclude session NAME and all descendants
    -T NAME      restrict to given theories (multiple options possible)

  Lint the session.
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
          "u:" -> (arg => options = options + ("update_" + arg)),
          "v" -> (_ => verbose = true),
          "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)),
          "T:" -> (arg => theories = theories ::: List(arg))
        )

        val session = getopts(args) match {
          case List(session_name) => session_name
          case _                  => getopts.usage()
        }
        val progress = new Console_Progress(verbose = verbose)
        progress.interrupt_handler {
          lint(
            options,
            session,
            theories,
            logic,
            progress = progress,
            dirs = dirs,
            select_dirs = select_dirs,
            verbose = verbose
          )
        }
      }
    )
}
