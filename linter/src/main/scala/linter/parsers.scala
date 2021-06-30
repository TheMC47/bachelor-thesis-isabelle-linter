package linter

import scala.util.parsing.combinator._
import scala.util.parsing.input
import isabelle._

import Linter._
object TokenParsers extends TokenParsers {

  case class IndexPosition(val ts: List[Text.Info[Token]], val i: Int) extends input.Position {
    def column: Int = ts.slice(0, i + 1).map(_.info.content.size).sum
    def line: Int = 0
    protected def lineContents: String = (ts map { _.info.content }).mkString
  }

  case class TokenReader(in: List[Text.Info[Token]], from: Int = 0) extends input.Reader[Text.Info[Token]] {
    def first: Text.Info[Token] = in.head
    def rest: TokenReader = TokenReader(in.tail, from + 1)
    def pos: input.Position = IndexPosition(in, from)
    def atEnd: Boolean = in.isEmpty
  }

}

trait TokenParsers extends Parsers {
  type Elem = Text.Info[Token]

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

  def is_atom(rtoken: Text.Info[Token]): Boolean = is_atom(rtoken.info)

  /* Token kinds */
  def pCommand(name: String): Parser[Elem] = elem(name, _.info.is_command(name))
  def pCommand(names: String*): Parser[Elem] = anyOf(names.map(pCommand(_)))

  def pSpace: Parser[Elem] = elem("space", _.info.is_space)

  def pKeyword(name: String): Parser[Elem] = elem(name, _.info.is_keyword(name))
  def pIdent: Parser[Elem] = elem("ident", _.info.is_ident)
  def pSymIdent: Parser[Elem] = elem("sym_ident", _.info.is_sym_ident)
  def pNat: Parser[Elem] = elem("nat", _.info.is_nat)
  def pString: Parser[Elem] = elem("string", _.info.is_string)

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
    elem("atom", (t => is_atom(t) && pred(t.info)))
  def pName: Parser[Elem] = elem("name", _.info.is_name)

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
      pNat ^^ (n => Method.Modifier.Restrict(n.info.content.toInt, n.range))
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

  /* Attributes */
  def pAttribute: Parser[List[Elem]] = pIdent.*

  def pAttributes: Parser[List[List[Elem]]] =
    chainl1[List[List[Elem]]](pAttribute ^^ { List(_) }, pKeyword(",") ^^^ { _ ::: _ })

  /* Putting things together.. */
  def pCatch: Parser[Unparsed] = pAny.* ^^ Unparsed

  def pAny: Parser[Elem] = elem("any", _ => true)

  def pNotParen: Parser[Elem] = elem("not_paren", t => !(t.info.is_keyword("(") || t.info.is_keyword(")")))

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

  def tryTransform[T](p: Parser[T], command: Parsed_Command): Option[T] =
    parse(p, command.tokens) match {
      case Success(result, next) => Some(result)
      case n: NoSuccess          => None
    }

  def mkString(tokens: List[Elem]): String = tokens.map(_.info.source).mkString

  def parse[T](p: Parser[T], in: List[Elem], keepSpaces: Boolean = false): ParseResult[T] = {
    val processed = if (keepSpaces) in else in.filterNot(_.info.is_space)
    p(TokenParsers.TokenReader(processed))
  }
}
