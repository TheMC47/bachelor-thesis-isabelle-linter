package linter

import isabelle._

object XML_Lint_Reporter {

  def report_lints(
      lint_results: List[Linter.Lint_Result],
      print_location: Boolean = false,
      print_name: Boolean = false
  ): XML.Body =
    lint_results.zipWithIndex
      .map(ri =>
        report_lint(
          ri._1,
          ri._2,
          print_location = print_location,
          print_name = print_name
        )
      )
      .flatten

  def report_lint(
      lint_result: Linter.Lint_Result,
      lint_number: Int = 0,
      print_location: Boolean = false,
      print_name: Boolean = false
  ): XML.Body = {

    val source = lint_result.commands.head.snapshot.node.source

    def text_range_to_line(range: Text.Range): Line.Range = Line.Document(source).range(range)

    val location = when(
      print_location,
      text(s"At ${text_range_to_line(lint_result.range).start.print}:\n")
    )

    val message = text(s" ${lint_number + 1}. ${lint_result.message}")
    val lint_name =
      when(print_name, text(s" [lint name: ${lint_result.lint_name}]"))

    val edit = lint_result.edit match {
      case Some(edit) => text(" Consider: ") ::: edit_markup(edit)
      case None       => Nil
    }

    block(location ::: message ::: edit ::: lint_name)
  }

  /* xml helpers */

  def when(b: Boolean, x: XML.Body): XML.Body = if (b) x else Nil

  def edit_markup(edit: Linter.Edit): XML.Body = XML.Elem(
    Markup(
      Markup.LINTER,
      Position.Range(edit.range) ::: Markup.Content(edit.replacement)
    ),
    text(edit.message)
  ) :: Nil

  def text(content: String): XML.Body = XML.Text(content) :: Nil

  def block(inner: XML.Body): XML.Body =
    XML.elem(Markup.Block.name, inner) :: Nil

}
