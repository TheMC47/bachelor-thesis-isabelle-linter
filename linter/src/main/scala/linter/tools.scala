package linter

import isabelle._

/* This enables SBT entry point. */
object Tools extends App

/* This enables Isabelle entry point. */
class Tools extends Isabelle_Scala_Tools(Linter_Tool.isabelle_tool)
