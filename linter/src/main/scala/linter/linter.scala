package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable

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
        commands.iterator foreach debugCommand
        def debugCommand(c: Command): Unit = {
          def markups(state: Command.State): List[Markup] = state.status
          // Print stuff that you think is useful
          val span = c.span
          progress.echo(c.source)
          progress.echo("----------")
          progress.echo("name: " + span.name)
          progress.echo("kind: " + span.kind.toString)
          progress.echo("position: " + span.position.toString)
          val tokens: List[Token] = span.content
          val s: List[String] = tokens.map(t => t.kind.toString + "-" + t.source)
          progress.echo(s.mkString("Tokens_sources(", ",", ")"))
          val cmd_states = snapshot.state.command_states(snapshot.version, c)
          progress.echo("Number of command states: " + cmd_states.length.toString())
          val markups_str = markups(cmd_states.head).map(_.name).mkString(",")
          progress.echo("Markups (for first cmd state): " + markups_str)
          progress.echo("##########")
        }
      }))

    context.check_errors

    // XXX THis is not valid anymore
    // Print the lints
    def report(node: ContextNode): Unit = node match {
      case Annotated_Node(
            children,
            Lint_Context(ast_node, span, xml, lint_result)
          ) => {
        lint_result map (span.report(progress, _))
        children map report
      }
    }

  }

  /* ==== Parsing ====
   * Working directly on the xml-markup is awkward. The idea is to construct an
   * abstract structure to make defining lints easy. This could be a simplified
   * AST that only tries to parse parts of the document that are relevant for
   * the linter.
   * */

  /* Structures for parsing */
  // For now, just the index of the xml node
  case class Span(startLine: Int, endLine: Int) {
    def report(progress: Progress, s: String): Unit = progress.echo(s"At line $startLine: $s")
  }

  case class Lint_Context(
      // Some(_) if the parser was succesful in finding the abstract
      // representation of the associted XML
      val node: Option[Abstract_Node],
      val span: Span,
      val xml: XML.Body,
      // Option[String] is just a placeholder. This should provide refactoring
      // suggestions, markup wrappers for jEdit integration, etc.
      val lint_result: Option[String]
  )

  // Is this useful? Not sure
  case class Annotated_Node[T](
      val children: List[Annotated_Node[T]],
      val context: T
  )

  type ContextNode = Annotated_Node[Lint_Context]

  object ContextNode {
    def apply(children: List[ContextNode], context: Lint_Context): ContextNode =
      Annotated_Node[Lint_Context](children, context)
  }

  /* The abstract structure that lints use */
  abstract class Abstract_Node
  abstract class Proof_Node extends Abstract_Node
  object Sorry extends Proof_Node

  def parse(xml: XML.Body): ContextNode = {

    // Helpers to easily find sorries
    class Named_Entity(val name: java.lang.String) {
      def unapply(markup: Markup): Boolean = markup match {
        case Markup.Entity(_, name) => name == this.name
        case _                      => false
      }
    }
    val SorryE = new Named_Entity("sorry")

    // For now, just find sorry
    def parse_one(t: XML.Tree): Option[Abstract_Node] = t match {
      case XML.Elem(SorryE(), _) => Some(Sorry)
      case _                     => None
    }

    val children = xml zip Stream.from(1) map {
      case (t, l: Int) => {
        val context = Lint_Context(parse_one(t), Span(l, l), List(t), None)
        ContextNode(List.empty, context)
      }
    }
    val context = Lint_Context(None, Span(1, xml.length), xml, None)

    ContextNode(children, context)
  }

  /* ==== Linting ====
   * The idea is to lint a ContextNode and update its Lint_Context. For now,
   * it's tries to lint the outermost nodes before visiting the children:
   * changing something at the parent node might change the subgoals for
   * example, rendering the lints at the children level useless. This could be
   * made more finegrained by defining a severity measure of a lint, and decide
   * whether it's worth it to continue linting the children.
   *
   * An abstract class Lint takes care of pattern matching the different types
   * of the abstract structure with the visit method. This makes writing the
   * lints easier, as they only need to visit the nodes they're interested in.
   *
   * */

  abstract class Lint {
    def visit_proof(node: Proof_Node): Option[String] = None

    def visit(node: Abstract_Node): Option[String] = node match {
      case proof: Proof_Node => visit_proof(proof)
      case _                 => None
    }

    final def lint(node: ContextNode): ContextNode = node match {
      case an @ Annotated_Node(children, Lint_Context(ast_node, span, xml, lint_result)) =>
        lint_result match {
          // If this is already linted, skip it
          case Some(_) => an
          case None => {
            val lint_result = ast_node flatMap visit //ast_node >>= visit

            // If there is a lint to be applied at the parent node, skip linting
            // the children
            val linted_children = lint_result match {
              case Some(_) => children
              case None    => children map lint
            }
            Annotated_Node(linted_children, Lint_Context(ast_node, span, xml, lint_result))
          }
        }
    }
  }

  object NoSorry extends Lint {
    override def visit_proof(node: Proof_Node): Option[String] = node match {
      case Sorry => Some("Sorry detected")
      case _     => None
    }
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
