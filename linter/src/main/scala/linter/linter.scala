package linter

import java.io.PrintWriter

import isabelle._
import scala.collection.immutable
import scala.util.parsing.combinator._
import scala.util.parsing.input

object Linter {

  type Lint_Report = String

  abstract class Lint {
    def lint(elem: DocumentElement): Option[Lint_Report]
  }

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

    commands.iterator
      .map(lint_command(_, lints))
      .flatten
      .toList
  }

  def lint_command(command: Command, lints: List[Lint]): Option[Lint_Report] =
    lints.toStream.map(_.lint(parse_command(command))).find(_.isDefined).flatten

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

  case class Sorry() extends Proof
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

    def pSorry: Parser[Sorry] = pCommand("sorry") ^^^ Sorry()

    def pCatch: Parser[Unparsed] = elem("any", _ => true).* ^^ Unparsed

    def tokenParser: Parser[DocumentElement] = pApply | pSorry | pCatch

    def parse[T](p: Parser[T], in: List[Token]): ParseResult[T] =
      p(TokenReader(in filterNot (_.is_space)))
  }

  object Print_Structure extends Lint {
    def lint(elem: DocumentElement): Option[Lint_Report] = Some(s"Parsed: $elem")
  }
}
