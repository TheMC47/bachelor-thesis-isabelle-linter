package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable
import isabelle.XML.Elem

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

    /* Debugging/exploration functions to bombard the terminal with XML */
    def indent_and_echo = (n: Int, s: String) => {
      progress.echo("  " * n + s)
      // Thread.sleep(100)
    }

    def decipher_m(markup: Markup, n: Int): Unit = {
      indent_and_echo(n, "NAME: " + markup.name)
    }

    def decipher(xml: XML.Body, n: Int = 0): Unit =
      xml foreach { elem =>
        elem match {
          // case Elem(SorryE(), body) => {
          //   indent_and_echo(n, "Sorry")
          //   decipher(body, n + 1)
          // }
          // case Elem(markup, body) => {
          //   indent_and_echo(n, "<= ELEM =>")
          //   decipher_m(markup, n)
          //   decipher(body, n + 1)
          //   indent_and_echo(n, "=> ELEM <=")
          // }
          // case XML.Text(content) => {
          //   indent_and_echo(n, "<= Text =>")
          //   indent_and_echo(n, content)
          //   indent_and_echo(n, ">= Text =<")
          // }
          case _ => ()
        }
      }

    /* ======= Actual code starts here */

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
        for (node_name <- snapshot.node_files) {
          val node = snapshot.get_node(node_name)
          val xml: XML.Body =
            snapshot.state.xml_markup(
              snapshot.version,
              node_name,
              elements = Markup.Elements(
                // Markup.KEYWORD,
                // Markup.KEYWORD1,
                // Markup.KEYWORD2,
                // Markup.KEYWORD3,
                // Markup.PLAIN_TEXT,
                // Markup.OPERATOR,
                // Markup.DELIMITER
                Markup.ENTITY
              )
            )
          // progress.echo(XML.string_of_body(xml))
          // decipher(xml)
          val parsed_xml: ContextNode = parse(xml)
          // val l = xml.length
          // progress.echo(s"Read: $l lines")
          val linted = lints.foldLeft(parsed_xml) { (context_node, lint) =>
            lint.lint(context_node)
          }
          report(linted)
          // I couldn't figure out how to use a particular theory, so I'm adding
          // a delay to manually stop the processing
          progress.echo("This is getting out of hand, now there i̶s̶ will be two of them")
          Thread.sleep(100000)
        }
      }))

    context.check_errors

    // Print the lints
    def report(node: ContextNode): Unit = node match {
      case Annotated_Node(children, Lint_Context(ast_node, span, xml, lint_result)) => {
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
      case Elem(SorryE(), _) => Some(Sorry)
      case _                 => None
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
