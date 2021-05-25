package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable
import scala.util.parsing.combinator._
import scala.util.parsing.input

object Linter {

  def debug_command(c: Command, progress: Progress): Unit = {
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
    progress.echo("##########")
  }

  def lint(
      snapshot: Document.Snapshot,
      lints: List[Lint],
      progress: Progress = new Progress // TODO Is this needed?
  ): List[Lint_Report] = {
    val commands = snapshot.node.commands

    commands.iterator foreach (debug_command(_, progress)) // Debugging

    val parsed_commands = commands.iterator.map(Parsed_Command(_, snapshot))

    parsed_commands
      .map(lint_command(_, lints))
      .flatten
      .toList
  }

  def lint_command(
      command: Command,
      snapshot: Document.Snapshot,
      lints: List[Lint]
  ): Option[Lint_Report] =
    lint_command(Parsed_Command(command, snapshot), lints)

  def lint_command(command: Parsed_Command, lints: List[Lint]): Option[Lint_Report] =
    lints.toStream.map(_.lint(command)).find(_.isDefined).flatten

  case class Ranged_Token(val token: Token, range: Line.Range) {
    /* Redefining functions from Token, to save a level of inderection */

    val content: String = token.content

    val source: String = token.source

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

    def unapply(r: Ranged_Token): Option[(Token.Kind.Value, String, Line.Range)] =
      Some(r.token.kind, r.source, r.range)

    def list_range(ranges: List[Line.Range]): Line.Range = ranges match {
      case rs @ (head :: _) => Line.Range(head.start, rs.last.stop)
      case Nil              => Line.Range.zero
    }
  }

  case class Parsed_Command(val command: Command, snapshot: Document.Snapshot) {
    /* Position Info */

    // We're certain that we don't receive a none here, since we're only using commands that are
    // guaranteed to be in the state.
    val node_pos: Line.Node_Position = snapshot.find_command_position(command.id, 0).get

    val file_name: String = node_pos.name

    val start_position: Line.Position = node_pos.pos

    val range: Line.Range =
      Line.Range(start_position, start_position.advance(command.source))

    /* Tokens with position */

    def generate_positions(
        tokens: List[Token],
        start_position: Line.Position
    ): List[Ranged_Token] = tokens match {
      case head :: next => {
        val end_position = start_position.advance(head.source)
        (Ranged_Token(head, Line.Range(start_position, end_position))) :: generate_positions(
          next,
          end_position
        )
      }
      case Nil => Nil
    }

    val tokens: List[Ranged_Token] =
      generate_positions(command.span.content, start_position)

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

  abstract class DocumentElement(val range: Line.Range)
  abstract class Proof(override val range: Line.Range) extends DocumentElement(range)
  case class Name(val content: String, override val range: Line.Range)
      extends DocumentElement(range)
  object Name {
    def apply(token: Ranged_Token): Name = Name(token.content, token.range)
  }

  case class Atom(val content: String, override val range: Line.Range)
      extends DocumentElement(range)
  object Atom {
    def apply(token: Ranged_Token): Atom = Atom(token.content, token.range)
  }

  abstract class Arg(override val range: Line.Range) extends DocumentElement(range)

  case class Single_Arg(val atom: Atom, override val range: Line.Range) extends Arg(range)
  object Single_Arg {
    def apply(atom: Atom): Single_Arg = Single_Arg(atom, atom.range)
  }

  case class Args(val args: List[Arg], override val range: Line.Range) extends Arg(range)
  object Args {
    def apply(args: List[Arg]): Args = Args(args, Ranged_Token.list_range(args.map(_.range)))
  }

  object Method {
    /* Modifiers */
    trait Modifier
    object Modifier {
      object Try extends Modifier // ?
      object Rep1 extends Modifier // +
      case class Restrict(val n: Int) extends Modifier // [n]
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
            Combined_Method(left, combinator, right, range, modifiers :+ value)
          case Simple_Method(name, range, modifiers, args) =>
            Simple_Method(name, range, modifiers :+ value, args)
        }
    }
  }

  abstract class Method(override val range: Line.Range) extends DocumentElement(range)

  case class Simple_Method(
      val name: Name,
      override val range: Line.Range,
      val modifiers: List[Method.Modifier] = Nil,
      val args: Arg = Args(Nil, Line.Range.zero)
  ) extends Method(range)
  object Simple_Method {

    def apply(
        name: Name,
        args: Arg
    ): Simple_Method =
      Simple_Method(name, name.range, Nil, args)

    def apply(name: Name): Simple_Method =
      Simple_Method(name, name.range, Nil, Args(Nil, Line.Range.zero))
  }

  case class Combined_Method(
      val left: Method,
      val combinator: Method.Combinator,
      val right: Method,
      override val range: Line.Range,
      val modifiers: List[Method.Modifier] = Nil
  ) extends Method(range)
  object Combined_Method {
    def apply(left: Method, combinator: Method.Combinator, right: Method): Combined_Method =
      Combined_Method(left, combinator, right, Line.Range(left.range.start, right.range.stop), Nil)
  }

  case class Apply(val method: Method, override val range: Line.Range) extends Proof(range)
  case class Isar_Proof(val method: Option[Method], override val range: Line.Range)
      extends Proof(range)
  case class Unparsed(val tokens: List[Ranged_Token]) extends DocumentElement(Line.Range.zero)
  case class Failed(val string: String) extends DocumentElement(Line.Range.zero)

  object TokenParsers extends Parsers {
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
    def pAtom(pred: Token => Boolean): Parser[Atom] =
      elem("atom", (t => is_atom(t) && pred(t.token))) ^^ (Atom(_))
    def pName: Parser[Name] = elem("name", _.is_name) ^^ (Name(_))

    /* Args */
    def pSingle_Arg(pred: Token => Boolean): Parser[Single_Arg] = pAtom(pred) ^^ (Single_Arg(_))
    def pArgs(pred: Token => Boolean): Parser[Args] =
      (pSqBracketed(pArg(pred).*) | pParened(pArg(pred).*)) ^^ (Args(_))

    def pArg: Parser[Arg] = pArg(_ => true)
    def pArg(pred: Token => Boolean): Parser[Arg] = pSingle_Arg(pred) | pArgs(pred)

    object MethodParsers {

      /* Modifiers */
      def pTry: Parser[Method.Modifier] = pKeyword("?") ^^^ Method.Modifier.Try
      def pRep1: Parser[Method.Modifier] = pKeyword("+") ^^^ Method.Modifier.Rep1
      def pRestrict: Parser[Method.Modifier] = pSqBracketed(
        pNat ^^ (n => Method.Modifier.Restrict(n.content.toInt))
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
      def pMethodArg: Parser[Arg] = pArg { token =>
        !(token.is_open_bracket ||
          token.is_close_bracket ||
          (token.is_keyword && "|;,+".exists(token.is_keyword)))
      }

      def pNameOnly: Parser[Simple_Method] = pName ^^ (name => Simple_Method(name))
      def pNameArgs: Parser[Simple_Method] = pName ~ pMethodArg.* ^^ { case name ~ args =>
        Simple_Method(name, args = Args(args))
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
        Apply(method, Line.Range(applyToken.range.start, method.range.stop))
    }

    /* Isar-Proof */
    def pIsarProof: Parser[Isar_Proof] = pCommand("proof") ~ MethodParsers.pMethod.? ^^ {
      case proofToken ~ Some(method) =>
        Isar_Proof(Some(method), Line.Range(proofToken.range.start, method.range.stop))
      case proofToken ~ None => Isar_Proof(None, proofToken.range)
    }

    /* Lemma attributes */
    def pAttribute: Parser[Elem] = pIdent

    def pAttributes: Parser[List[Elem]] =
      chainl1[List[Elem]](pAttribute ^^ { List(_) }, pKeyword(",") ^^^ { _ ::: _ })

    /* Putting things together.. */
    def pCatch: Parser[Unparsed] = elem("any", _ => true).* ^^ Unparsed

    def tokenParser: Parser[DocumentElement] = pApply | pIsarProof | pCatch

    def parse[T](p: Parser[T], in: List[Elem]): ParseResult[T] =
      p(TokenReader(in filterNot (_.is_space)))
  }

  /* ==== Linting ====
   * A Lint needs to define a function, lint, that takes a Parsed_Command and optionally returns a
   * Lint_Report. This is further refined by other abstract classes, that provide interafces that
   * are more convenient.
   * */

  case class Lint_Report(
      val lint_name: String,
      val message: String,
      val range: Line.Range,
      val edit: Option[(String, String)],
      command: Parsed_Command
  ) {
    val file_name: String = command.file_name
  }

  type Reporter = (String, Line.Range, Option[(String, String)]) => Some[Lint_Report]

  sealed trait Lint {

    // The name of the lint. snake_case
    val name: String

    def lint(command: Parsed_Command): Option[Lint_Report] = {
      lint(
        command,
        (message, range, edit) => Some(Lint_Report(name, message, range, edit, command))
      )
    }

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Report]
  }

  /* Lints that use raw commands
   * */
  abstract class Raw_Command_Lint extends Lint {
    def lint_command(
        command: Command,
        report: Reporter
    ): Option[Lint_Report]

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Report] =
      lint_command(command.command, report)
  }

  object Debug_Command extends Lint {

    val name: String = "debug_command"

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Report] = {
      report(s"${command.tokens}", command.range, None)
    }
  }

  /* Lints that use a raw token stream
   * */

  abstract class Raw_Token_Stream_Lint extends Lint {
    def lint(tokens: List[Ranged_Token], report: Reporter): Option[Lint_Report]

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Report] =
      lint(command.tokens, report)
  }

  object Axiomatization_With_Where extends Raw_Token_Stream_Lint {

    val name: String = "axiomatization_with_where"

    def lint(tokens: List[Ranged_Token], report: Reporter): Option[Lint_Report] = tokens match {
      case Ranged_Token(Token.Kind.COMMAND, "axiomatization", range) :: next
          if next.exists(_.content == "where") =>
        report("Don't use axiomatization", range, None)
      case _ => None
    }
  }

  abstract class Illegal_Command_Lint(
      message: String,
      lint_name: String,
      illegal_commands: List[String]
  ) extends Raw_Token_Stream_Lint {

    val name: String = lint_name

    def lint(tokens: List[Ranged_Token], report: Reporter): Option[Lint_Report] = tokens match {
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
  abstract class Parser_Lint extends Lint {

    def parser(report: Reporter): TokenParsers.Parser[Some[Lint_Report]]

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Report] =
      TokenParsers.parse(parser(report), command.tokens) match {
        case TokenParsers.Success(result, _) => result
        case _                               => None
      }
  }

  object Short_Name extends Parser_Lint {
    import TokenParsers._

    val name: String = "name_too_short"

    override def parser(report: Reporter): TokenParsers.Parser[Some[Lint_Report]] =
      pCommand("fun", "definition") ~> elem("ident", _.content.size < 2) ^^ (token =>
        report(s"""Name "${token.content}" too short""", token.range, None)
      )
  }

  object Unnamed_Lemma_Simplifier_Attributes extends Parser_Lint {
    import TokenParsers._

    val name: String = "simplifier_on_unnamed_lemma"

    override def parser(report: Reporter): Parser[Some[Lint_Report]] =
      pCommand("lemma") ~> pSqBracketed(pAttributes)
        .map(_.find(attr => List("simp", "cong").contains(attr.content)))
        .withFilter(_.isDefined)
        .map(_.get)
        .map(ranged_token =>
          report("Don't use simplifier attributes on unnamed lemmas", ranged_token.range, None)
        )
  }

  object Lemma_Transforming_Attributes extends Parser_Lint {
    import TokenParsers._

    val name: String = "lemma_transforming_attributes"

    override def parser(report: Reporter): Parser[Some[Lint_Report]] =
      (pCommand("lemma") ~ pIdent.?) ~> pSqBracketed(pAttributes)
        .map(_.find(attr => List("simplified", "rule_format").contains(attr.content)))
        .withFilter(_.isDefined)
        .map(_.get)
        .map(ranged_token =>
          report("Don't use transforming attributes on lemmas", ranged_token.range, None)
        )
  }

  /* Lints that use the parsed document structure
   * */
  abstract class Structure_Lint extends Lint {

    def lint_apply(method: Method, report: Reporter): Option[Lint_Report] = None

    def lint_isar_proof(method: Option[Method], report: Reporter): Option[Lint_Report] = None

    def lint_proof(proof: Proof, report: Reporter): Option[Lint_Report] = proof match {
      case Apply(method, _)      => lint_apply(method, report)
      case Isar_Proof(method, _) => lint_isar_proof(method, report)
    }

    def lint_document_element(elem: DocumentElement, report: Reporter): Option[Lint_Report] =
      elem match {
        case p: Proof => lint_proof(p, report)
        case _        => None
      }

    def lint(command: Parsed_Command, report: Reporter): Option[Lint_Report] =
      lint_document_element(command.parsed, report)

  }

  object Implicit_Rule extends Structure_Lint {

    val name: String = "implicit_rule"

    override def lint_apply(method: Method, report: Reporter): Option[Lint_Report] = method match {
      case Simple_Method(Name("rule", _), range, _, Args(Nil, _)) =>
        report("Do not use implicit rule", range, None)
      case Combined_Method(left, _, right, _, _) =>
        lint_apply(left, report).orElse(lint_apply(right, report))
      case _ => None
    }
  }

  object Simple_Isar_Method extends Structure_Lint {

    val name: String = "complex_isar_proof"

    def is_complex(method: Method): Boolean = method match {
      case Simple_Method(Name(name, _), _, _, _) => name == "auto"
      case Combined_Method(left, _, right, _, _) => is_complex(left) || is_complex(right)
    }

    override def lint_isar_proof(method: Option[Method], report: Reporter): Option[Lint_Report] =
      for {
        s_method <- method
        if is_complex(method.get)
      } yield report("Keep initial proof methods simple", s_method.range, None).get
  }

  object Print_Structure extends Structure_Lint {

    val name: String = "print_structure"

    override def lint_document_element(
        elem: DocumentElement,
        report: Reporter
    ): Option[Lint_Report] =
      report(s"Parsed: $elem", Line.Range.zero, None)
  }

  val all_lints: List[Lint] = List(
    Axiomatization_With_Where,
    Proof_Finder,
    Counter_Example_Finder,
    Bad_Style_Command,
    Diagnostic_Command,
    Short_Name,
    Unnamed_Lemma_Simplifier_Attributes,
    Lemma_Transforming_Attributes,
    Implicit_Rule,
    Simple_Isar_Method,
    // Debugging lints
    // Print_Structure,
    // Debug_Command
  )
}
