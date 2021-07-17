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
    Low_Level_Apply_Chain,
    Use_Isar,
    Debug_Command,
    Axiomatization_With_Where,
    Unfinished_Proof,
    Proof_Finder,
    Counter_Example_Finder,
    Bad_Style_Command,
    Diagnostic_Command,
    Short_Name,
    Global_Attribute_On_Unnamed_Lemma,
    Lemma_Transforming_Attribute,
    Implicit_Rule,
    Complex_Isar_Initial_Method,
    Force_Failure,
    Auto_Structural_Composition,
    Print_Structure,
    Complex_Method,
    Global_Attribute_Changes
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
        Low_Level_Apply_Chain.name,
        Complex_Isar_Initial_Method.name
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
        Global_Attribute_On_Unnamed_Lemma.name,
        Implicit_Rule.name,
        Complex_Isar_Initial_Method.name
      )
    )

    val pedantic = Bundle(
      "pedantic",
      Set(
        Low_Level_Apply_Chain.name,
        Use_Isar.name,
        Short_Name.name,
        Force_Failure.name,
        Auto_Structural_Composition.name
      )
    )

    val default = Bundle(
      "default",
      Set(
        Apply_Isar_Switch.name,
        Use_By.name,
        Unrestricted_Auto.name,
        Low_Level_Apply_Chain.name,
        Axiomatization_With_Where.name,
        Bad_Style_Command.name,
        Short_Name.name,
        Global_Attribute_On_Unnamed_Lemma.name,
        Lemma_Transforming_Attribute.name,
        Implicit_Rule.name,
        Complex_Isar_Initial_Method.name
      )
    )
  }

  private val bundle_store: Map[String, Bundle] = Map.empty

  def register_bundle(bundle: Bundle): Unit =
    bundle_store += ((bundle.name, bundle))

  def get_bundle(name: String): Option[Bundle] =
    bundle_store.get(name)

  private val all_bundles =
    List(Bundle.isar, Bundle.no_diagnosis, Bundle.foundational, Bundle.pedantic, Bundle.default)

  for (bundle <- all_bundles) register_bundle(bundle)
}
