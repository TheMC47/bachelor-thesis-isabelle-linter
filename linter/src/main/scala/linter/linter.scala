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
    val parseResult = TokenParsers.parse(TokenParsers.tokenParser, tokens) match {
      case TokenParsers.Success(result, next) => s"Success. Parsed: $result, left: $next"
      case _                                  => "Failed"
    }
    progress.echo(s"Parse Result: $parseResult")
    progress.echo("##########")
  }

  def lint(
      snapshot: Document.Snapshot,
      lints: List[Lint],
      progress: Progress = new Progress // TODO Is this needed?
  ) {
    val commands = snapshot.node.commands

    commands.iterator foreach (debug_command(_, progress)) // Debugging
  }
  /* ==== Parsing ====
   * Try to map token streams into something that has more structure.
   * */

  case class TokenReader(in: List[Token]) extends input.Reader[Token] {
    def first: Token = in.head
    def rest: TokenReader = TokenReader(in.tail)
    def pos: input.Position = input.NoPosition
    def atEnd: Boolean = in.isEmpty
  }

  abstract class DocumentElement
  abstract class Proof extends DocumentElement
  case class Sorry() extends Proof
  case class Apply() extends Proof
  case class Unparsed(val tokens: List[Token]) extends DocumentElement

  object TokenParsers extends Parsers {
    type Elem = Token

    def pCommand(name: String): Parser[Elem] = elem(name, _.is_command(name))

    def pSorry: Parser[Sorry] = pCommand("sorry") ^^^ Sorry()

    def pCatch: Parser[Unparsed] = elem("any", _ => true).* ^^ (Unparsed(_))

    def tokenParser: Parser[DocumentElement] = pSorry | pCatch

    def parse[T](p: Parser[T], in: List[Token]): ParseResult[T] =
      p(TokenReader(in filterNot (_.is_space)))
  }

  }
}
