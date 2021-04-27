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
      .map(c => {
        val parseResult = parse_command(c)
        lints.toStream.map(_.lint(parseResult)).find(_.isDefined).map(_.get)
      })
      .filter(_.isDefined)
      .map(_.get)
      .toList
  }
  /* ==== Parsing ====
   * Try to map token streams into something that has more structure.
   * */

  def parse_command(command: Command): DocumentElement =
    TokenParsers.parse(TokenParsers.tokenParser, command.span.content) match {
      case TokenParsers.Success(result, TokenReader(Nil)) => result
      case TokenParsers.Success(_, next)                  => error(s"Failed parsing. $next left")
      case failure: TokenParsers.NoSuccess                => error(failure.msg)
    }

  case class TokenReader(in: List[Token]) extends input.Reader[Token] {
    def first: Token = in.head
    def rest: TokenReader = TokenReader(in.tail)
    def pos: input.Position = input.NoPosition
    def atEnd: Boolean = in.isEmpty
  }

  abstract class DocumentElement
  abstract class Proof extends DocumentElement
  case class Name(val content: String) extends DocumentElement
  case class Atom(val content: String) extends DocumentElement

  trait Arg extends DocumentElement
  case class Single_Arg(val atom: Atom) extends Arg
  case class Args(val args: List[Arg]) extends Arg
  case class Sorry() extends Proof
  case class Apply() extends Proof
  case class Unparsed(val tokens: List[Token]) extends DocumentElement

  object TokenParsers extends Parsers {
    type Elem = Token

    def is_atom(token: Token): Boolean = token.is_name ||
      token.kind == Token.Kind.TYPE_IDENT ||
      token.kind == Token.Kind.TYPE_VAR ||
      token.kind == Token.Kind.VAR || // term_var
      token.is_nat ||
      token.is_float ||
      token.is_keyword ||
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

    /* Building blocks */
    def pAtom(pred: Token => Boolean): Parser[Atom] =
      elem("atom", (t => is_atom(t) && pred(t))) ^^ (token => Atom(token.content))
    def pName: Parser[Name] = elem("name", _.is_name) ^^ (token => Name(token.content))

    /* Args */
    def pArg: Parser[Arg] = pArg(_ => true)
    // Atoms can be too general, so propagate a predicate
    def pArg(pred: Token => Boolean): Parser[Arg] =
      (pAtom(pred) ^^ Single_Arg) | ((pSqBracketed(pArg(pred).*) | pParened(pArg(pred).*)) ^^ Args)

    def pSorry: Parser[Sorry] = pCommand("sorry") ^^^ Sorry()

    def pCatch: Parser[Unparsed] = elem("any", _ => true).* ^^ (Unparsed(_))

    def tokenParser: Parser[DocumentElement] = pSorry | pCatch

    def parse[T](p: Parser[T], in: List[Token]): ParseResult[T] =
      p(TokenReader(in filterNot (_.is_space)))
  }

  object Print_Structure extends Lint {
    def lint(elem: DocumentElement): Option[Lint_Report] = Some(s"Parsed: $elem")
  }
}
