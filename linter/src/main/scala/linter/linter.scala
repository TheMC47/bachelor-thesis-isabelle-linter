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

    val parsed_commands = commands.iterator.map(Parsed_Command)

    parsed_commands
      .map(lint_command(_, lints))
      .flatten
      .toList
  }

  def lint_command(command: Command, lints: List[Lint]): Option[Lint_Report] =
    lint_command(Parsed_Command(command), lints)

  def lint_command(command: Parsed_Command, lints: List[Lint]): Option[Lint_Report] =
    lints.toStream.map(_.lint(command)).find(_.isDefined).flatten

  case class Parsed_Command(val command: Command) {
    lazy val parsed: DocumentElement = parse_command(command)
  }

  /* ==== Parsing ====
   * Try to map token streams into something that has more structure.
   * */

  def parse_command(command: Command): DocumentElement =
    TokenParsers.parse(TokenParsers.tokenParser, command.span.content) match {
      case TokenParsers.Success(result, TokenReader(Nil, _)) => result
      case TokenParsers.Success(_, next)                     => Failed(s"Failed parsing. $next left")
      case failure: TokenParsers.NoSuccess                   => Failed(failure.msg)
    }

  case class IndexPosition(val ts: List[Token], val i: Int) extends input.Position {
    def column: Int = ts.slice(0, i + 1).map(_.content.size).sum
    def line: Int = 0
    protected def lineContents: String = (ts map { _.content }).mkString
  }

  case class TokenReader(in: List[Token], from: Int = 0) extends input.Reader[Token] {
    def first: Token = in.head
    def rest: TokenReader = TokenReader(in.tail, from + 1)
    def pos: input.Position = IndexPosition(in, from)
    def atEnd: Boolean = in.isEmpty
  }

  abstract class DocumentElement
  abstract class Proof extends DocumentElement
  case class Name(val content: String) extends DocumentElement
  case class Atom(val content: String) extends DocumentElement

  trait Arg extends DocumentElement
  case class Single_Arg(val atom: Atom) extends Arg
  case class Args(val args: List[Arg]) extends Arg

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
          case Combined_Method(left, combinator, right, modifiers) =>
            Combined_Method(left, combinator, right, modifiers :+ value)
          case Simple_Method(name, modifiers, args) => Simple_Method(name, modifiers :+ value, args)
        }
    }
  }

  abstract class Method extends DocumentElement
  case class Simple_Method(
      val name: Name,
      val modifiers: List[Method.Modifier] = Nil,
      val args: Arg = Args(Nil)
  ) extends Method

  case class Combined_Method(
      val left: Method,
      val combinator: Method.Combinator,
      val right: Method,
      val modifiers: List[Method.Modifier] = Nil
  ) extends Method

  case class Apply(val method: Method) extends Proof
  case class Unparsed(val tokens: List[Token]) extends DocumentElement
  case class Failed(val string: String) extends DocumentElement

  object TokenParsers extends Parsers {
    type Elem = Token

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
      (token.is_keyword &&
        !token.is_open_bracket &&
        !token.is_close_bracket &&
        !token.is_keyword("|") &&
        !token.is_keyword(",") &&
        !token.is_keyword(";")) ||
      token.kind == Token.Kind.CARTOUCHE

    /* Token kinds */
    def pCommand(name: String): Parser[Token] = elem(name, _.is_command(name))
    def pCommand(names: String*): Parser[Token] = anyOf(names.map(pCommand(_)))

    def pKeyword(name: String): Parser[Token] = elem(name, _.is_keyword(name))
    def pIdent: Parser[Token] = elem("ident", _.is_ident)
    def pSymIdent: Parser[Token] = elem("sym_ident", _.is_sym_ident)
    def pNat: Parser[Token] = elem("nat", _.is_nat)
    def pString: Parser[Token] = elem("string", _.is_string)

    /* Surrounded parsers */
    def pSurrounded[T, U](left: Parser[T], right: Parser[T])(center: Parser[U]): Parser[U] =
      left ~> center <~ right
    def pOpenSqBracket: Parser[Token] = pKeyword("[")
    def pClosedSqBracket: Parser[Token] = pKeyword("]")
    def pSqBracketed[U]: Parser[U] => Parser[U] = pSurrounded(pOpenSqBracket, pClosedSqBracket)

    def pOpenParen: Parser[Token] = pKeyword("(")
    def pClosedParen: Parser[Token] = pKeyword(")")
    def pParened[U]: Parser[U] => Parser[U] = pSurrounded(pOpenParen, pClosedParen)

    /* Simple elements */

    // Atoms can be too general, so propagate a predicate
    def pAtom(pred: Token => Boolean): Parser[Atom] =
      elem("atom", (t => is_atom(t) && pred(t))) ^^ (token => Atom(token.content))

    def pName: Parser[Name] = elem("name", _.is_name) ^^ (token => Name(token.content))

    def pArg: Parser[Arg] = pArg(_ => true)
    def pArg(pred: Token => Boolean): Parser[Arg] =
      (pAtom(pred) ^^ Single_Arg) | ((pSqBracketed(pArg(pred).*) | pParened(pArg(pred).*)) ^^ Args)

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
    def pApply: Parser[Apply] = pCommand("apply") ~> MethodParsers.pMethod ^^ Apply

    def pCatch: Parser[Unparsed] = elem("any", _ => true).* ^^ Unparsed

    def tokenParser: Parser[DocumentElement] = pApply | pCatch

    def parse[T](p: Parser[T], in: List[Token]): ParseResult[T] =
      p(TokenReader(in filterNot (_.is_space)))
  }

  /* ==== Linting ====
   * A Lint needs to define a function, lint, that takes a Parsed_Command and optionally returns a
   * Lint_Report. This is further refined by other abstract classes, that provide interafces that
   * are more convenient.
   * */

  type Lint_Report = String

  sealed trait Lint {
    def lint(command: Parsed_Command): Option[Lint_Report]
  }

  /* Lints that use raw commands
   * */
  abstract class Raw_Command_Lint extends Lint {
    def lint_command(command: Command): Option[Lint_Report]

    def lint(command: Parsed_Command): Option[Lint_Report] = lint_command(command.command)
  }

  /* Lints that use a raw token stream
   * */

  abstract class Raw_Token_Stream_Lint extends Lint {
    def lint_token_stream(tokens: List[Token]): Option[Lint_Report]

    def lint(command: Parsed_Command): Option[Lint_Report] = lint_token_stream(
      command.command.span.content
    )
  }

  abstract class Illegal_Command_Lint(message: String, illegal_commands: List[String])
      extends Raw_Token_Stream_Lint {
    def lint_token_stream(tokens: List[Token]): Option[Lint_Report] = tokens match {
      case head :: _ if (illegal_commands.contains(head.content)) => Some(message)
      case _                                                      => None
    }
  }

  object Unfinished_Proof
      extends Illegal_Command_Lint("Unfinished proof", List("sorry", "oops", "\\<proof>"))

  object Proof_Finder
      extends Illegal_Command_Lint(
        "Proof finder",
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
        List(
          "nitpick",
          "nunchaku",
          "quickcheck"
        )
      )

  object Bad_Style_Command
      extends Illegal_Command_Lint("Bad style command", List("back", "apply_end"))

  object Diagnostic_Command
      extends Illegal_Command_Lint(
        "Interactive diagnostic command",
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

    def parser: TokenParsers.Parser[Lint_Report]

    def lint(command: Parsed_Command): Option[Lint_Report] =
      TokenParsers.parse(parser, command.command.span.content) match {
        case TokenParsers.Success(result, _) => Some(result)
        case _                               => None
      }
  }

  object Short_Name extends Parser_Lint {
    import TokenParsers._

    def parser: TokenParsers.Parser[Lint_Report] =
      pCommand("fun", "definition") ~> elem("ident", _.content.size < 2) ^^ (token =>
        s"""Name "${token.content}" too short"""
      )
  }

  /* Lints that use the parsed document structure
   * */
  abstract class Structure_Lint extends Lint {

    def lint_apply(method: Method): Option[Lint_Report] = None

    def lint_proof(proof: Proof): Option[Lint_Report] = proof match {
      case Apply(method) => lint_apply(method)
    }

    def lint_document_element(elem: DocumentElement): Option[Lint_Report] = elem match {
      case p: Proof => lint_proof(p)
      case _        => None
    }

    def lint(command: Parsed_Command): Option[Lint_Report] = lint_document_element(command.parsed)

  }

  object Implicit_Rule extends Structure_Lint {
    override def lint_apply(method: Method): Option[Lint_Report] = method match {
      case Simple_Method(Name("rule"), _, Args(Nil)) => Some("Do not use implicit rule")
      case Combined_Method(left, _, right, _)        => lint_apply(left).orElse(lint_apply(right))
      case _                                         => None
    }
  }

  object Print_Structure extends Structure_Lint {
    override def lint_document_element(elem: DocumentElement): Option[Lint_Report] =
      Some(s"Parsed: $elem")
  }
}
