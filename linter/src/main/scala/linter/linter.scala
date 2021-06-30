package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable

object Linter {

  def mapAccumL[A, B, C](xs: List[A], acc: B, result: (A, B) => (C, B)): List[C] = xs match {
    case Nil => Nil
    case head :: next =>
      result(head, acc) match {
        case (y, new_acc) => y :: mapAccumL(next, new_acc, result)
      }
  }

  def lint(
      snapshot: Document.Snapshot,
      configuration: Linter_Configuration
  ): Lint_Report = {

    val commands = snapshot.node.commands.iterator.toList
    val parsed_commands = mapAccumL[Command, Text.Offset, Parsed_Command](
      commands,
      0,
      { case (command, offset) =>
        val parsed_command = Parsed_Command(command, snapshot, offset)
        (parsed_command, parsed_command.range.stop)
      }
    )

    configuration.get_lints.foldLeft(Lint_Report.empty)((report, lint) =>
      lint.lint(parsed_commands, report)
    )
  }

  object RToken {

    def unapply(r: Text.Info[Token]): Option[(Token.Kind.Value, String, Text.Range)] =
      Some(r.info.kind, r.info.source, r.range)

    def apply(token: Token, offset: Text.Offset): Text.Info[Token] =
      Text.Info(Text.Range(0, token.source.length()) + offset, token)
  }

  def list_range(ranges: List[Text.Range]): Text.Range = ranges match {
    case _ :: _ => Text.Range(ranges.head.start, ranges.last.stop)
    case Nil    => Text.Range.offside
  }

  object Parsed_Command {
    def unapply(command: Parsed_Command): Option[String] = Some(command.kind)
  }

  case class Parsed_Command(
      val command: Command,
      snapshot: Document.Snapshot,
      offset: Text.Offset
  ) {
    val node_name: Document.Node.Name = snapshot.node_name

    val kind: String = command.span.kind.toString()

    val range: Text.Range = command.range + offset

    /* Tokens with position */

    def generate_positions(
        tokens: List[Token],
        start_offset: Text.Offset
    ): List[Text.Info[Token]] = mapAccumL[Token, Text.Offset, Text.Info[Token]](
      tokens,
      start_offset,
      {
        case (token, offset) => {
          val rtoken = RToken(token, offset)
          (rtoken, rtoken.range.stop)
        }
      }
    )

    val tokens: List[Text.Info[Token]] =
      generate_positions(command.span.content, offset)

    /* ==== Parsing ====
     * Try to map token streams into something that has more structure.
     * */

    lazy val parsed: DocumentElement =
      TokenParsers.parse(TokenParsers.tokenParser, tokens) match {
        case TokenParsers.Success(result, TokenParsers.TokenReader(Nil, _)) => result
        case TokenParsers.Success(_, next)                                  => Failed(s"Failed parsing. $next left")
        case failure: TokenParsers.NoSuccess                                => Failed(failure.msg)
      }

  }

  abstract class DocumentElement(val range: Text.Range)
  abstract class Proof(override val range: Text.Range) extends DocumentElement(range)

  object Method {
    /* Modifiers */
    abstract class Modifier(val range: Text.Range)
    object Modifier {
      case class Try(override val range: Text.Range) extends Modifier(range) // ?
      case class Rep1(override val range: Text.Range) extends Modifier(range) // +
      case class Restrict(val n: Int, override val range: Text.Range) extends Modifier(range) // [n]
    }

    /* Combinators */

    trait Combinator
    object Combinator {
      object Seq extends Combinator // ,
      object Struct extends Combinator // ;
      object Alt extends Combinator // |
    }

    def addModifier(method: Method, modifier: Option[Modifier]): Method = modifier match {
      case None => method
      case Some(value) =>
        method match {
          case Combined_Method(left, combinator, right, range, modifiers) =>
            Combined_Method(
              left,
              combinator,
              right,
              Text.Range(range.start, value.range.stop),
              modifiers :+ value
            )
          case Simple_Method(name, range, modifiers, args) =>
            Simple_Method(name, Text.Range(range.start, value.range.stop), modifiers :+ value, args)
        }
    }
  }

  abstract class Method(override val range: Text.Range) extends DocumentElement(range)

  case class Simple_Method(
      val name: Text.Info[Token],
      override val range: Text.Range,
      val modifiers: List[Method.Modifier] = Nil,
      val args: List[Text.Info[Token]] = Nil
  ) extends Method(range)
  object Simple_Method {

    def apply(
        name: Text.Info[Token],
        args: List[Text.Info[Token]]
    ): Simple_Method =
      Simple_Method(name, name.range, Nil, args)

    def apply(name: Text.Info[Token]): Simple_Method =
      Simple_Method(name, name.range, Nil, Nil)
  }

  case class Combined_Method(
      val left: Method,
      val combinator: Method.Combinator,
      val right: Method,
      override val range: Text.Range,
      val modifiers: List[Method.Modifier] = Nil
  ) extends Method(range)
  object Combined_Method {
    def apply(left: Method, combinator: Method.Combinator, right: Method): Combined_Method =
      Combined_Method(left, combinator, right, Text.Range(left.range.start, right.range.stop), Nil)
  }

  case class Apply(val method: Method, override val range: Text.Range) extends Proof(range)
  case class Isar_Proof(val method: Option[Method], override val range: Text.Range)
      extends Proof(range)
  case class Unparsed(val tokens: List[Text.Info[Token]]) extends DocumentElement(Text.Range(0))
  case class Failed(val string: String) extends DocumentElement(Text.Range(0))

  /* ==== Linting ====
   * A Lint needs to define a function, lint, that takes a Parsed_Command and optionally returns a
   * Lint_Result. This is further refined by other abstract classes, that provide interafces that
   * are more convenient.
   * */

  case class Lint_Result(
      val lint_name: String,
      val message: String,
      val range: Text.Range,
      val edit: Option[Edit],
      val severity: Severity.Value,
      command: Parsed_Command
  ) {
    val node_name: Document.Node.Name = command.node_name
  }

  object Lint_Report {
    val empty: Lint_Report = Lint_Report(Nil)
  }

  case class Lint_Report(val results: List[Lint_Result]) {

    def add_result(result: Lint_Result): Lint_Report = Lint_Report(result :: results)

    def command_lint(id: Document_ID.Command): List[Lint_Result] =
      results.filter(_.command.command.id == id)
  }

  case class Edit(val range: Text.Range, val replacement: String, val msg: Option[String] = None) {
    val message: String = msg.getOrElse(replacement)
  }

  type Reporter = (String, Text.Range, Option[Edit]) => Some[Lint_Result]

  object Severity extends Enumeration {
    type Level = Value
    val LOW, MEDIUM, HIGH = Value
  }

  object Category extends Enumeration {
    type Name = Value
    val readability, maintenance, style = Value
  }

  sealed trait Lint {

    // The name of the lint. snake_case
    val name: String
    // Severity of the lint
    val severity: Severity.Level
    // Category
    val category: Category.Name

    def lint(commands: List[Parsed_Command], report: Lint_Report): Lint_Report

  }

  abstract class Proper_Commands_Lint extends Lint {
    def lint(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
      lint_proper(commands.filter(_.command.is_proper), report)

    def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report

    def add_result(
        message: String,
        range: Text.Range,
        edit: Option[Edit],
        command: Parsed_Command,
        report: Lint_Report
    ): Lint_Report =
      report.add_result(
        Lint_Result(
          name,
          message,
          range,
          edit,
          severity,
          command
        )
      )
  }

  abstract class Single_Command_Lint extends Lint {

    def lint(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
      commands
        .map(command =>
          lint(
            command,
            (message, range, edit) =>
              Some(Lint_Result(name, message, range, edit, severity, command))
          )
        )
        .flatten
        .foldLeft(report)((report, result) => report.add_result(result))

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result]
  }

  /* Lints that use raw commands
   * */
  abstract class Raw_Command_Lint extends Single_Command_Lint {
    def lint_command(
        command: Command,
        report: Reporter
    ): Option[Lint_Result]

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] =
      lint_command(command.command, report)
  }

  /* Lints that use a raw token stream
   * */

  abstract class Raw_Token_Stream_Lint extends Single_Command_Lint {
    def lint(tokens: List[Text.Info[Token]], report: Reporter): Option[Lint_Result]

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] =
      lint(command.tokens, report)
  }

  /* Lints that are parsers
   * */
  abstract class Parser_Lint extends Single_Command_Lint with TokenParsers {

    def parser(report: Reporter): Parser[Some[Lint_Result]]

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] =
      parse(parser(report), command.tokens) match {
        case Success(result, _) => result
        case _                  => None
      }
  }

  /* Lints that use the parsed document structure
   * */
  abstract class Structure_Lint extends Single_Command_Lint {

    def lint_apply(method: Method, report: Reporter): Option[Lint_Result] = None

    def lint_isar_proof(method: Option[Method], report: Reporter): Option[Lint_Result] = None

    def lint_proof(proof: Proof, report: Reporter): Option[Lint_Result] = proof match {
      case Apply(method, _)      => lint_apply(method, report)
      case Isar_Proof(method, _) => lint_isar_proof(method, report)
    }

    def lint_document_element(elem: DocumentElement, report: Reporter): Option[Lint_Result] =
      elem match {
        case p: Proof => lint_proof(p, report)
        case _        => None
      }

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] =
      lint_document_element(command.parsed, report)

  }
}
