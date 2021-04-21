package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable
import scala.util.parsing.combinator._
import scala.util.parsing.input

object Linter {

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

    val lints = getLints()

    // ... and use the context to get the sessions, and then the markup
    context.build_logic(logic)
    context
      .sessions(logic, log = log)
      .foreach(_.process((args: Dump.Args) => {
        progress.echo("Processing theory " + args.print_node + " ...")

        val snapshot = args.snapshot
        val node = snapshot.node
        val commands = node.commands
        commands.iterator foreach (c => {
          // Print stuff that you think is useful
          def markups(state: Command.State): List[Markup] = state.status
          val span = c.span
          progress.echo(c.source)
          progress.echo("----------")
          progress.echo("name: " + span.name)
          progress.echo("kind: " + span.kind.toString)
          progress.echo("position: " + span.position.toString)
          val tokens: List[Token] = span.content filterNot (_.is_space)
          val s: List[String] = tokens.map(t => t.kind.toString + "-" + t.source)
          progress.echo(s.mkString("Tokens_sources(", ",", ")"))
          val cmd_states = snapshot.state.command_states(snapshot.version, c)
          progress.echo("Number of command states: " + cmd_states.length.toString())
          val markups_str = markups(cmd_states.head).map(_.name).mkString(",")
          progress.echo("Markups (for first cmd state): " + markups_str)
          val parseResult = TokenParsers.parse(TokenParsers.tokenParser, tokens) match {
            case TokenParsers.Success(result, next) => s"Success. Parsed: $result, left: $next"
            case _                                  => "Failed"
          }
          progress.echo(s"Parse Result: $parseResult")
          progress.echo("##########")
        })
      }))

    context.check_errors
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
  def getLints(): List[Lint] = List(NoSorry)

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
    -u OPT       overide update option: shortcut for "-o update_OPT"
    -v           verbose
    -x NAME      exclude session NAME and all descendants

  Update theory sources based on PIDE markup.
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
