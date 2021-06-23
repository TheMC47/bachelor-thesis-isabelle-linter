package linter

import Linter._

import isabelle._
import scala.annotation.tailrec

object Apply_Isar_Switch extends Proper_Commands_Lint {

  val name = "apply_isar_switch"

  @tailrec
  def lint_proper(commands: List[Parsed_Command], report: Lint_Report): Lint_Report =
    commands match {
      case Parsed_Command("apply") :: (proof @ Parsed_Command("proof")) :: next => {
        val new_report = report.add_result(
          Lint_Result(
            name,
            "Do not switch between apply-style and ISAR proofs.",
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

object Use_By extends Proper_Commands_Lint with TokenParsers {

  val name: String = "use_by"

  def pRemoveApply: Parser[String] = (pCommand("apply") ~ pSpace.?) ~> pAny.* ^^ mkString


  private def edits(apply_script: List[Parsed_Command]): String =
    apply_script match {
      case apply1 :: apply2 :: done :: Nil => {
        val first = doParseTransform(pRemoveApply)(apply1)
        val second = doParseTransform(pRemoveApply)(apply2)
        s"by $first $second"
      }
      case apply :: done :: Nil => {
        val no_paren = doParseTransform(pRemoveApply)(apply)
        s"by $no_paren"
      }
      case _ => error("Expected two or three commands")
    }

  private def report_lint(apply_script: List[Parsed_Command], report: Lint_Report): Lint_Report =
    report.add_result(
      Lint_Result(
        name,
        """Use "by" instead of a short apply-script.""",
        apply_script.head.range,
        Some(Edit(list_range(apply_script map (_.range)), edits(apply_script))),
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
        "Do not use unrestricted auto as a non-terminal proof method.",
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
            "Compress low-level proof methods into automated search.",
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

object Use_Isar extends Single_Command_Lint {

  val name: String = "use_isar"

  def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] = command match {
    case (c @ Parsed_Command("apply")) =>
      report("Use Isar instead of apply-scripts.", c.range, None)
    case _ => None
  }
}

object Debug_Command extends Single_Command_Lint {

  val name: String = "debug_command"

  def lint(command: Parsed_Command, report: Reporter): Option[Lint_Result] = {
    report(s"${command.tokens}", command.range, None)
  }
}

object Axiomatization_With_Where extends Raw_Token_Stream_Lint {

  val name: String = "axiomatization_with_where"

  def lint(tokens: List[Ranged_Token], report: Reporter): Option[Lint_Result] = tokens match {
    case Ranged_Token(Token.Kind.COMMAND, "axiomatization", range) :: next =>
      next.dropWhile(_.source != "where") match {
        case xs @ (_ :: _) =>
          report(
            """Do not use axiomatization with a where clause.""",
            Text.Range(xs.head.range.start, xs.last.range.stop),
            Some(Edit(list_range(xs.map(_.range)), "", Some("Remove where")))
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
      report(
        message,
        head.range,
        Some(Edit(list_range(tokens.map(_.range)), "", Some("Remove invocation")))
      )
    case _ => None
  }
}

object Unfinished_Proof
    extends Illegal_Command_Lint(
      "Consider finishing the proof.",
      "unfinished_proof_command",
      List("sorry", "oops", "\\<proof>")
    )

object Proof_Finder
    extends Illegal_Command_Lint(
      "Remove proof finder command.",
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
      "Remove counter-example finder command.",
      "counter_example_finder_command",
      List(
        "nitpick",
        "nunchaku",
        "quickcheck"
      )
    )

object Bad_Style_Command
    extends Illegal_Command_Lint(
      "Bad style command.",
      "bad_style_command",
      List("back", "apply_end")
    )

object Diagnostic_Command
    extends Illegal_Command_Lint(
      "Remove interactive diagnostic command",
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

object Short_Name extends Parser_Lint {

  val name: String = "name_too_short"

  override def parser(report: Reporter): Parser[Some[Lint_Result]] =
    pCommand("fun", "definition") ~> elem("ident", _.content.size < 2) ^^ (token =>
      report(s"""Name "${token.content}" is too short.""", token.range, None)
    )
}

object Unnamed_Lemma_Simplifier_Attributes extends Parser_Lint {

  val name: String = "simplifier_on_unnamed_lemma"

  override def parser(report: Reporter): Parser[Some[Lint_Result]] =
    pCommand("lemma") ~> pSqBracketed(pAttributes) >> {
      _.find(List("simp", "cong") contains _.content) match {
        case None => failure("no match")
        case Some(token) =>
          success(report("Do not use simplifier attributes on unnamed lemmas.", token.range, None))
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
          success(report("Do not use transforming attributes on lemmas.", token.range, None))
      }
    }
}

object Implicit_Rule extends Structure_Lint {

  val name: String = "implicit_rule"

  override def lint_apply(method: Method, report: Reporter): Option[Lint_Result] = method match {
    case Simple_Method(Ranged_Token(_, "rule", _), range, _, Nil) =>
      report("Do not use implicit rule.", range, None)
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
    } yield report("Keep initial proof methods simple.", s_method.range, None).get
}

object Force_Failure extends Structure_Lint {
  val name: String = "force_failure"

  override def lint_apply(method: Method, report: Reporter): Option[Lint_Result] = method match {
    case Simple_Method(Ranged_Token(_, "simp", _), range, modifiers, args) =>
      report("Consider forciing failure.", range, None)
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

object Lints {

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
    Simple_Isar_Method,
    Unfinished_Proof
    // Debugging lints
    // Print_Structure,
    // Debug_Command
  )
}
