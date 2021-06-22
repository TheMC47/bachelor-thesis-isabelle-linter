package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable
import scala.util.parsing.combinator._
import scala.util.parsing.input

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

    def pNotParen: Parser[Elem] = elem("not_paren", t => !(t.is_keyword("(") || t.is_keyword(")")))

    def pWithParen(p: Parser[List[Elem]]): Parser[List[Elem]] = pOpenParen ~ p ~ pClosedParen ^^ {
      case r ~ ts ~ l => (r :: ts) :+ l
    }

    def pAnyBalanced: Parser[List[Elem]] = chainl1(
      pNotParen.*,
      pWithParen(pAnyBalanced) ^^ { mid => (l: List[Elem], r: List[Elem]) =>
        l ::: mid ::: r
      }
    )

    def tokenParser: Parser[DocumentElement] = pApply | pIsarProof | pCatch

    def doParseTransform(p: Parser[String])(command: Parsed_Command): String =
      parse(p, command.tokens, true) match {
        case Success(result, next) => result
        case n: NoSuccess          => error(s"Failed: $n \n ${command.tokens}")
      }

    def mkString(tokens: List[Elem]): String = tokens.map(_.source).mkString

    def parse[T](p: Parser[T], in: List[Elem], keepSpaces: Boolean = false): ParseResult[T] = {
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
      val edit: Option[Edit],
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
    def lint(tokens: List[Ranged_Token], report: Reporter): Option[Lint_Result]

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
