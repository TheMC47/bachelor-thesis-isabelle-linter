package linter

import Linter._
import scala.collection.mutable.Map

object Lint_Store {

  private val store: Map[String, Lint] = Map.empty

  def register_lint(lint: Lint): Unit =
    store += ((lint.name, lint))

  def get_lint(lint_name: String): Option[Lint] = store.get(lint_name)

  private val all_lints: List[Lint] = List(
    Apply_Isar_Switch,
    Use_By,
    Unrestricted_Auto,
    Single_Step_Low_Level_Apply,
    Use_Isar,
    Debug_Command,
    Axiomatization_With_Where,
    Unfinished_Proof,
    Proof_Finder,
    Counter_Example_Finder,
    Bad_Style_Command,
    Diagnostic_Command,
    Short_Name,
    Unnamed_Lemma_Simplifier_Attributes,
    Lemma_Transforming_Attributes,
    Implicit_Rule,
    Simple_Isar_Method,
    Force_Failure,
    Apply_Auto_Struct,
    Print_Structure
  )

  for (lint <- all_lints) register_lint(lint)

  // bundles
  case class Bundle(val name: String, val lint_names: Set[String])

  object Bundle {
    /* basic bundles */
    val isar = Bundle(
      "isar",
      Set(
        Apply_Isar_Switch.name,
        Use_By.name,
        Use_Isar.name,
        Single_Step_Low_Level_Apply.name,
        Simple_Isar_Method.name
      )
    )

    val no_diagnosis = Bundle(
      "no_diagnosis",
      Set(
        Unfinished_Proof.name,
        Proof_Finder.name,
        Counter_Example_Finder.name,
        Diagnostic_Command.name
      )
    )

    val foundational = Bundle(
      "foundational",
      Set(
        Apply_Isar_Switch.name,
        Use_By.name,
        Unrestricted_Auto.name,
        Bad_Style_Command.name,
        Unnamed_Lemma_Simplifier_Attributes.name,
        Implicit_Rule.name,
        Simple_Isar_Method.name
      )
    )

    val pedantic = Bundle(
      "pedantic",
      Set(
        Single_Step_Low_Level_Apply.name,
        Use_Isar.name,
        Short_Name.name,
        Force_Failure.name,
        Apply_Auto_Struct.name
      )
    )

    val default = Bundle(
      "default",
      Set(
        Apply_Isar_Switch.name,
        Use_By.name,
        Unrestricted_Auto.name,
        Single_Step_Low_Level_Apply.name,
        Axiomatization_With_Where.name,
        Bad_Style_Command.name,
        Short_Name.name,
        Unnamed_Lemma_Simplifier_Attributes.name,
        Lemma_Transforming_Attributes.name,
        Implicit_Rule.name,
        Simple_Isar_Method.name,
      )
    )
  }

  private val bundle_store: Map[String, Bundle] = Map.empty

  def bundle_register(bundle: Bundle): Unit =
    bundle_store += ((bundle.name, bundle))

  def get_bundle(name: String): Option[Bundle] =
    bundle_store.get(name)

  private val all_bundles =
    List(Bundle.always, Bundle.isar, Bundle.no_diagnosis, Bundle.foundational, Bundle.interactive)

  for (bundle <- all_bundles) bundle_register(bundle)
}
