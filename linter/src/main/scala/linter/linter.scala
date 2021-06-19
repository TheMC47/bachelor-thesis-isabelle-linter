package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable
import scala.util.parsing.combinator._
import scala.util.parsing.input
import scala.annotation.tailrec

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
      lints: List[Lint]
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

    lints.foldLeft(Lint_Report.empty)((report, lint) => lint.lint(parsed_commands, report))
  }

  case class Ranged_Token(val token: Token, offset: Text.Offset) {
    /* Redefining functions from Token, to save a level of inderection */

    lazy val content: String = token.content

    val source: String = token.source

    val range: Text.Range = Text.Range(0, source.length()) + offset

    def is_command: Boolean = token.is_command
    def is_command(name: String): Boolean = token.is_command(name)
    def is_keyword: Boolean = token.is_keyword
    def is_keyword(name: String): Boolean = token.is_keyword(name)
    def is_keyword(name: Char): Boolean = token.is_keyword(name)
    def is_delimiter: Boolean = token.is_delimiter
    def is_ident: Boolean = token.is_ident
    def is_sym_ident: Boolean = token.is_sym_ident
    def is_string: Boolean = token.is_string
    def is_nat: Boolean = token.is_nat
    def is_float: Boolean = token.is_float
    def is_name: Boolean = token.is_name
    def is_embedded: Boolean = token.is_embedded
    def is_text: Boolean = token.is_text
    def is_space: Boolean = token.is_space
    def is_informal_comment: Boolean = token.is_informal_comment
    def is_formal_comment: Boolean = token.is_formal_comment
    def is_marker: Boolean = token.is_marker
    def is_comment: Boolean = token.is_comment
    def is_ignored: Boolean = token.is_ignored
    def is_proper: Boolean = token.is_proper
    def is_error: Boolean = token.is_error
    def is_unparsed: Boolean = token.is_unparsed
    def is_unfinished: Boolean = token.is_unfinished
    def is_open_bracket: Boolean = token.is_open_bracket
    def is_close_bracket: Boolean = token.is_close_bracket
    def is_begin: Boolean = token.is_begin
    def is_end: Boolean = token.is_end
    def is_begin_or_command: Boolean = token.is_begin_or_command
    def is_system_name: Boolean = token.is_system_name
  }

  object Ranged_Token {

    def unapply(r: Ranged_Token): Option[(Token.Kind.Value, String, Text.Range)] =
      Some(r.token.kind, r.source, r.range)

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
    ): List[Ranged_Token] = mapAccumL[Token, Text.Offset, Ranged_Token](
      tokens,
      start_offset,
      {
        case (token, offset) => {
          val ranged_token = Ranged_Token(token, offset)
          (ranged_token, ranged_token.range.stop)
        }
      }
    )

    val tokens: List[Ranged_Token] =
      generate_positions(command.span.content, offset)

    /* ==== Parsing ====
     * Try to map token streams into something that has more structure.
     * */

    lazy val parsed: DocumentElement =
      TokenParsers.parse(TokenParsers.tokenParser, tokens) match {
        case TokenParsers.Success(result, TokenReader(Nil, _)) => result
        case TokenParsers.Success(_, next)                     => Failed(s"Failed parsing. $next left")
        case failure: TokenParsers.NoSuccess                   => Failed(failure.msg)
      }

  }

  case class IndexPosition(val ts: List[Ranged_Token], val i: Int) extends input.Position {
    def column: Int = ts.slice(0, i + 1).map(_.content.size).sum
    def line: Int = 0
    protected def lineContents: String = (ts map { _.content }).mkString
  }

  case class TokenReader(in: List[Ranged_Token], from: Int = 0) extends input.Reader[Ranged_Token] {
    def first: Ranged_Token = in.head
    def rest: TokenReader = TokenReader(in.tail, from + 1)
    def pos: input.Position = IndexPosition(in, from)
    def atEnd: Boolean = in.isEmpty
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
      val name: Ranged_Token,
      override val range: Text.Range,
      val modifiers: List[Method.Modifier] = Nil,
      val args: List[Ranged_Token] = Nil
  ) extends Method(range)
  object Simple_Method {

    def apply(
        name: Ranged_Token,
        args: List[Ranged_Token]
    ): Simple_Method =
      Simple_Method(name, name.range, Nil, args)

    def apply(name: Ranged_Token): Simple_Method =
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
  case class Unparsed(val tokens: List[Ranged_Token]) extends DocumentElement(Text.Range(0))
  case class Failed(val string: String) extends DocumentElement(Text.Range(0))

  trait TokenParsers extends Parsers {
    type Elem = Ranged_Token

    /* Utilities */

    /* Like chainl1, but parses q at least once.
     * */
    def chainl2[T](p: => Parser[T], q: => Parser[(T, T) => T]): Parser[T] = chainl2(p, p, q)

    def chainl2[T, U](
        first: => Parser[T],
        p: => Parser[U],
        q: => Parser[(T, U) => T]
    ): Parser[T] = first ~ rep1(q ~ p) ^^ { case x ~ xs =>
      xs.foldLeft(x) { case (a, f ~ b) =>
        f(a, b)
      }
    }

    def anyOf[T](ps: => Seq[Parser[T]]): Parser[T] = ps.reduce(_ | _)

    def is_atom(token: Token): Boolean = token.is_name ||
      token.kind == Token.Kind.TYPE_IDENT ||
      token.kind == Token.Kind.TYPE_VAR ||
      token.kind == Token.Kind.VAR || // term_var
      token.is_nat ||
      token.is_float ||
      token.is_keyword ||
      token.kind == Token.Kind.CARTOUCHE

    def is_atom(rtoken: Ranged_Token): Boolean = is_atom(rtoken.token)

    /* Token kinds */
    def pCommand(name: String): Parser[Elem] = elem(name, _.is_command(name))
    def pCommand(names: String*): Parser[Elem] = anyOf(names.map(pCommand(_)))

    def pSpace: Parser[Elem] = elem("space", _.is_space)

    def pKeyword(name: String): Parser[Elem] = elem(name, _.is_keyword(name))
    def pIdent: Parser[Elem] = elem("ident", _.is_ident)
    def pSymIdent: Parser[Elem] = elem("sym_ident", _.is_sym_ident)
    def pNat: Parser[Elem] = elem("nat", _.is_nat)
    def pString: Parser[Elem] = elem("string", _.is_string)

    /* Surrounded parsers */
    def pSurrounded[T, U](left: Parser[T], right: Parser[T])(center: Parser[U]): Parser[U] =
      left ~> center <~ right
    def pOpenSqBracket: Parser[Elem] = pKeyword("[")
    def pClosedSqBracket: Parser[Elem] = pKeyword("]")
    def pSqBracketed[U]: Parser[U] => Parser[U] = pSurrounded(pOpenSqBracket, pClosedSqBracket)

    def pOpenParen: Parser[Elem] = pKeyword("(")
    def pClosedParen: Parser[Elem] = pKeyword(")")
    def pParened[U]: Parser[U] => Parser[U] = pSurrounded(pOpenParen, pClosedParen)

    /* Simple elements */

    // Atoms can be too general, so propagate a predicate
    def pAtom(pred: Token => Boolean): Parser[Elem] =
      elem("atom", (t => is_atom(t) && pred(t.token)))
    def pName: Parser[Elem] = elem("name", _.is_name)

    /* Args */
    def pSingle_Arg(pred: Token => Boolean): Parser[List[Elem]] = pAtom(pred) ^^ { List(_) }
    def pArgs(pred: Token => Boolean): Parser[List[Elem]] =
      (pSqBracketed(pArg(pred).*) | pParened(pArg(pred).*)) ^^ { _.flatten }

    def pArg: Parser[List[Elem]] = pArg(_ => true)
    def pArg(pred: Token => Boolean): Parser[List[Elem]] = pSingle_Arg(pred) | pArgs(pred)

    object MethodParsers {

      /* Modifiers */
      def pTry: Parser[Method.Modifier] = pKeyword("?") ^^ { token =>
        Method.Modifier.Try(token.range)
      }
      def pRep1: Parser[Method.Modifier] = pKeyword("+") ^^ { token =>
        Method.Modifier.Rep1(token.range)
      }
      def pRestrict: Parser[Method.Modifier] = pSqBracketed(
        pNat ^^ (n => Method.Modifier.Restrict(n.content.toInt, n.range))
      )
      def pModifier: Parser[Method.Modifier] = pTry | pRep1 | pRestrict

      /* Combinators  and combined methods */
      def pCombinator(sep: String, comb: Method.Combinator): Parser[Method] =
        chainl2(
          pMethodInner,
          pKeyword(sep) ^^^ { (left: Method, right: Method) => Combined_Method(left, comb, right) }
        )

      def pAlt: Parser[Method] = pCombinator("|", Method.Combinator.Alt)
      def pSeq: Parser[Method] = pCombinator(",", Method.Combinator.Seq)
      def pStruct: Parser[Method] = pCombinator(";", Method.Combinator.Struct)

      /* Simple Methods */
      def pMethodArg: Parser[List[Elem]] = pArg { token =>
        !(token.is_open_bracket ||
          token.is_close_bracket ||
          (token.is_keyword && "|;,+".exists(token.is_keyword)))
      }

      def pNameOnly: Parser[Simple_Method] = pName ^^ (name => Simple_Method(name))
      def pNameArgs: Parser[Simple_Method] = pName ~ pMethodArg.* ^^ { case name ~ args =>
        Simple_Method(name, args = args.flatten)
      }

      /* Method */
      def pMethod: Parser[Method] =
        (pNameOnly | pParened(pMethods)) ~ pModifier.? ^^ { case body ~ modifier =>
          Method.addModifier(body, modifier)
        }

      // Following the railroad diagram, some methods like `(rule exI; auto)` will not get parsed
      // since `rule exI` is not inside parentheses. pMethod should only be used for parsing first
      // level methods.
      def pMethodInner: Parser[Method] = (pNameArgs | pParened(pMethods)) ~ pModifier.? ^^ {
        case body ~ modifier =>
          Method.addModifier(body, modifier)
      }

      def pMethods: Parser[Method] = pAlt | pStruct | pSeq | pNameArgs | pMethod
    }

    /* Apply */
    def pApply: Parser[Apply] = pCommand("apply") ~ MethodParsers.pMethod ^^ {
      case applyToken ~ method =>
        Apply(method, Text.Range(applyToken.range.start, method.range.stop))
    }

    /* Isar-Proof */
    def pIsarProof: Parser[Isar_Proof] = pCommand("proof") ~ MethodParsers.pMethod.? ^^ {
      case proofToken ~ Some(method) =>
        Isar_Proof(Some(method), Text.Range(proofToken.range.start, method.range.stop))
      case proofToken ~ None => Isar_Proof(None, proofToken.range)
    }

    /* Lemma attributes */
    def pAttribute: Parser[Elem] = pIdent

    def pAttributes: Parser[List[Elem]] =
      chainl1[List[Elem]](pAttribute ^^ { List(_) }, pKeyword(",") ^^^ { _ ::: _ })

    /* Putting things together.. */
    def pCatch: Parser[Unparsed] = pAny.* ^^ Unparsed

    def pAny: Parser[Elem] = elem("any", _ => true)

    def tokenParser: Parser[DocumentElement] = pApply | pIsarProof | pCatch

    def parse[T](p: Parser[T], in: List[Elem], keepSpaces: Boolean= true): ParseResult[T] = {
      val processed = if (keepSpaces) in else in.filterNot(_.is_space)
      p(TokenReader(processed))
    }
  }

  object TokenParsers extends TokenParsers

  /* ==== Linting ====
   * A Lint needs to define a function, lint, that takes a Parsed_Command and optionally returns a
   * Lint_Result. This is further refined by other abstract classes, that provide interafces that
   * are more convenient.
   * */

  case class Lint_Result(
      val lint_name: String,
      val message: String,
      val range: Text.Range,
      val edit: Option[(String, String)],
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

  type Reporter = (String, Text.Range, Option[(String, String)]) => Some[Lint_Result]

  sealed trait Lint {

    // The name of the lint. snake_case
    val name: String

    def lint(commands: List[Parsed_Command], report: Lint_Report): Lint_Report

  }

  abstract class Proper_Commands_Lint extends Lint {
    def lint(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
      lint_proper(commands.filter(_.command.is_proper), report)

    def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report
  }

  object Apply_Isar_Switch extends Proper_Commands_Lint {

    val name = "apply_isar_switch"

    @tailrec
    def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
      commands match {
        case Parsed_Command("apply") :: (proof @ Parsed_Command("proof")) :: next => {
          val new_report = report.add_result(
            Lint_Result(
              name,
              "Do not switch between apply-style and ISAR proofs",
              proof.range,
              None,
              proof
            )
          )
          lint_proper(next, new_report)
        }
        case _ :: next => lint_proper(next, report)
        case Nil       => report
      }
  }

  object Use_By extends Proper_Commands_Lint {

    val name: String = "use_by"

    private def report_lint(apply_script: List[Parsed_Command], report: Lint_Report): Lint_Report =
      report.add_result(
        Lint_Result(
          name,
          "Use by instead",
          apply_script.head.range,
          None,
          apply_script.head
        )
      )

    @tailrec
    def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
      commands match {
        case Parsed_Command("lemma")
            :: (apply1 @ Parsed_Command("apply"))
            :: (apply2 @ Parsed_Command("apply"))
            :: (done @ Parsed_Command("done"))
            :: next =>
          lint_proper(next, report_lint(apply1 :: apply2 :: done :: Nil, report))
        case Parsed_Command("lemma")
            :: (apply @ Parsed_Command("apply"))
            :: (done @ Parsed_Command("done"))
            :: next =>
          lint_proper(next, report_lint(apply :: done :: Nil, report))
        case _ :: next => lint_proper(next, report)
        case Nil       => report
      }

  }

  object Unrestricted_Auto extends Proper_Commands_Lint {
    val name: String = "unrestricted_auto"

    private def is_terminal(command: Parsed_Command): Boolean =
      List("sorry", "oops", "done", "\\<proof>").contains(command.kind)

    private def are_unrestricted(modifiers: List[Method.Modifier]): Boolean =
      !modifiers.exists(_.isInstanceOf[Method.Modifier.Restrict])

    private def is_unrestricted_auto__method(method: Method): Boolean = method match {

      case Simple_Method(name, range, modifiers, args) =>
        name.content == "auto" && are_unrestricted(modifiers)

      case _ => false
    }

    private def is_unrestricted_auto(element: DocumentElement): Boolean = element match {
      case Apply(method, range) => is_unrestricted_auto__method(method)
      case _                    => false
    }

    private def report_lint(apply: Parsed_Command, report: Lint_Report): Lint_Report =
      report.add_result(
        Lint_Result(
          name,
          "Do not use unrestricted auto as a non-terminal proof method",
          apply.range,
          None,
          apply
        )
      )

    @tailrec
    def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
      commands match {
        case (apply @ Parsed_Command("apply")) :: next_command :: next
            if !is_terminal(next_command) && is_unrestricted_auto(apply.parsed) =>
          lint_proper(next_command :: next, report_lint(apply, report))
        case _ :: next => lint_proper(next, report)
        case Nil       => report
      }
  }

  object Single_Step_Low_Level_Apply extends Proper_Commands_Lint {

    val name: String = "low_level_chain"

    private def is_low_level_method(method: Method): Boolean = method match {
      case Simple_Method(name, range, modifiers, args) =>
        List("erule", "rule", "simp", "clarsimp", "rule_tac").contains(name.content)
      case _ => false
    }

    private def is_low_level_apply(command: Parsed_Command): Boolean = command.parsed match {
      case Apply(method, range) => is_low_level_method(method)
      case _                    => false
    }

    def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report = {
      val (low_level_commands, rest) =
        commands.dropWhile(!is_low_level_apply(_)).span(is_low_level_apply(_))

      val new_report =
        if (low_level_commands.length >= 5)
          report.add_result(
            Lint_Result(
              name,
              "Consider compressing low level proof methods into automated search",
              low_level_commands.head.range,
              None,
              low_level_commands.head
            )
          )
        else
          report

      if (rest.isEmpty)
        new_report
      else lint_proper(rest, new_report)
    }
  }

  abstract class Single_Command_Lint extends Lint {

    def lint(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
      commands
        .map(command =>
          lint(
            command,
            (message, range, edit) => Some(Lint_Result(name, message, range, edit, command))
          )
        )
        .flatten
        .foldLeft(report)((report, result) => report.add_result(result))

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result]
  }

  object Use_Isar extends Single_Command_Lint {

    val name: String = "use_isar"

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] = command match {
      case (c @ Parsed_Command("apply")) =>
        report("Use Isar instead of apply-scripts", c.range, None)
      case _ => None
    }
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

  object Debug_Command extends Single_Command_Lint {

    val name: String = "debug_command"

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] = {
      report(s"${command.tokens}", command.range, None)
    }
  }

  /* Lints that use a raw token stream
   * */

  abstract class Raw_Token_Stream_Lint extends Single_Command_Lint {
    def lint(tokens: List[Ranged_Token], report: Reporter): Option[Lint_Result]

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] =
      lint(command.tokens, report)
  }

  object Axiomatization_With_Where extends Raw_Token_Stream_Lint {

    val name: String = "axiomatization_with_where"

    def lint(tokens: List[Ranged_Token], report: Reporter): Option[Lint_Result] = tokens match {
      case Ranged_Token(Token.Kind.COMMAND, "axiomatization", range) :: next =>
        next.dropWhile(_.source != "where") match {
          case xs @ (_ :: _) =>
            report(
              "Don't use axiomatization",
              Text.Range(xs.head.range.start, xs.last.range.stop),
              None
            )
          case Nil => None
        }
      case _ => None
    }
  }

  abstract class Illegal_Command_Lint(
      message: String,
      lint_name: String,
      illegal_commands: List[String]
  ) extends Raw_Token_Stream_Lint {

    val name: String = lint_name

    def lint(tokens: List[Ranged_Token], report: Reporter): Option[Lint_Result] = tokens match {
      case head :: _ if (illegal_commands.contains(head.content)) =>
        report(message, head.range, Some(tokens.map(_.source).mkString, ""))
      case _ => None
    }
  }

  object Unfinished_Proof
      extends Illegal_Command_Lint(
        "Unfinished proof",
        "unfinished_proof_command",
        List("sorry", "oops", "\\<proof>")
      )

  object Proof_Finder
      extends Illegal_Command_Lint(
        "Proof finder",
        "proof_finder_command",
        List(
          "sledgehammer",
          "solve_direct",
          "try",
          "try0"
        )
      )

  object Counter_Example_Finder
      extends Illegal_Command_Lint(
        "Counter example finder",
        "counter_example_finder_command",
        List(
          "nitpick",
          "nunchaku",
          "quickcheck"
        )
      )

  object Bad_Style_Command
      extends Illegal_Command_Lint(
        "Bad style command",
        "bad_style_command",
        List("back", "apply_end")
      )

  object Diagnostic_Command
      extends Illegal_Command_Lint(
        "Interactive diagnostic command",
        "diagnostic_command",
        List(
          "ML_val",
          "class_deps",
          "code_deps",
          "code_thms",
          "find_consts",
          "find_theorems",
          "find_unused_assms",
          "full_prf",
          "help",
          "locale_deps",
          "prf",
          "print_ML_antiquotations",
          "print_abbrevs",
          "print_antiquotations",
          "print_attributes",
          "print_bnfs",
          "print_bundles",
          "print_case_translations",
          "print_cases",
          "print_claset",
          "print_classes",
          "print_codeproc",
          "print_codesetup",
          "print_coercions",
          "print_commands",
          "print_context",
          "print_definitions",
          "print_defn_rules",
          "print_facts",
          "print_induct_rules",
          "print_inductives",
          "print_interps",
          "print_locale",
          "print_locales",
          "print_methods",
          "print_options",
          "print_orders",
          "print_quot_maps",
          "print_quotconsts",
          "print_quotients",
          "print_quotientsQ3",
          "print_quotmapsQ3",
          "print_record",
          "print_rules",
          "print_simpset",
          "print_state",
          "print_statement",
          "print_syntax",
          "print_term_bindings",
          "print_theorems",
          "print_theory",
          "print_trans_rules",
          "smt_status",
          "thm_deps",
          "thm_oracles",
          "thy_deps",
          "unused_thms",
          "value",
          "values",
          "welcome",
          "term",
          "prop",
          "thm",
          "typ"
        )
      )

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

  object Short_Name extends Parser_Lint {

    val name: String = "name_too_short"

    override def parser(report: Reporter): Parser[Some[Lint_Result]] =
      pCommand("fun", "definition") ~> elem("ident", _.content.size < 2) ^^ (token =>
        report(s"""Name "${token.content}" too short""", token.range, None)
      )
  }

  object Unnamed_Lemma_Simplifier_Attributes extends Parser_Lint {

    val name: String = "simplifier_on_unnamed_lemma"

    override def parser(report: Reporter): Parser[Some[Lint_Result]] =
      pCommand("lemma") ~> pSqBracketed(pAttributes) >> {
        _.find(List("simp", "cong") contains _.content) match {
          case None => failure("no match")
          case Some(token) =>
            success(report("Don't use simplifier attributes on unnamed lemmas", token.range, None))
        }
      }
  }

  object Lemma_Transforming_Attributes extends Parser_Lint {

    val name: String = "lemma_transforming_attributes"

    override def parser(report: Reporter): Parser[Some[Lint_Result]] =
      (pCommand("lemma") ~ pIdent.?) ~> pSqBracketed(pAttributes) >> {
        _.find(List("simplified", "rule_format") contains _.content) match {
          case None => failure("no match")
          case Some(token) =>
            success(report("Don't use transforming attributes on lemmas", token.range, None))
        }
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

  object Implicit_Rule extends Structure_Lint {

    val name: String = "implicit_rule"

    override def lint_apply(method: Method, report: Reporter): Option[Lint_Result] = method match {
      case Simple_Method(Ranged_Token(_, "rule", _), range, _, Nil) =>
        report("Do not use implicit rule", range, None)
      case Combined_Method(left, _, right, _, _) =>
        lint_apply(left, report).orElse(lint_apply(right, report))
      case _ => None
    }
  }

  object Simple_Isar_Method extends Structure_Lint {

    val name: String = "complex_isar_proof"

    def is_complex(method: Method): Boolean = method match {
      case Simple_Method(Ranged_Token(_, name, _), _, _, _) => name == "auto"
      case Combined_Method(left, _, right, _, _)            => is_complex(left) || is_complex(right)
    }

    override def lint_isar_proof(method: Option[Method], report: Reporter): Option[Lint_Result] =
      for {
        s_method <- method
        if is_complex(method.get)
      } yield report("Keep initial proof methods simple", s_method.range, None).get
  }

  object Force_Failure extends Structure_Lint {
    val name: String = "force_failure"

    override def lint_apply(method: Method, report: Reporter): Option[Lint_Result] = method match {
      case Simple_Method(Ranged_Token(_, "simp", _), range, modifiers, args) =>
        report("Consider forciing failure", range, None)
      case _ => None
    }
  }

  object Apply_Auto_Struct extends Structure_Lint {
    val name: String = "auto_structure_composition"

    private def has_auto(method: Method): Boolean = method match {
      case Simple_Method(name, range, modifiers, args) => name.source == "auto"
      case Combined_Method(left, combinator, right, range, modifiers) =>
        has_auto(left) || has_auto(right)

    }

    override def lint_apply(method: Method, report: Reporter): Option[Lint_Result] = method match {
      case Simple_Method(name, range, modifiers, args) => None
      case Combined_Method(left, Method.Combinator.Struct, right, range, modifiers) =>
        if (has_auto(left)) report("Do not use apply (auto;...)", range, None) else None
      case Combined_Method(left, _, right, range, modifiers) =>
        lint_apply(left, report).orElse(lint_apply(right, report))
    }
  }

  object Print_Structure extends Structure_Lint {

    val name: String = "print_structure"

    override def lint_document_element(
        elem: DocumentElement,
        report: Reporter
    ): Option[Lint_Result] =
      report(s"Parsed: $elem", elem.range, None)
  }

  val all_lints: List[Lint] = List(
    Apply_Isar_Switch,
    Use_By,
    Unrestricted_Auto,
    Single_Step_Low_Level_Apply,
    // Force_Failure,
    // Use_Isar,
    Apply_Auto_Struct,
    Axiomatization_With_Where,
    Proof_Finder,
    Counter_Example_Finder,
    Bad_Style_Command,
    Diagnostic_Command,
    Short_Name,
    Unnamed_Lemma_Simplifier_Attributes,
    Lemma_Transforming_Attributes,
    Implicit_Rule,
    Simple_Isar_Method
    // Debugging lints
    // Print_Structure,
    // Debug_Command
  )
}
