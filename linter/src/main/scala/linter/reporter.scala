package linter

import isabelle._

abstract class Reporter[A] {
  def report_for_command(lint_results: Linter.Lint_Report, id: Document_ID.Command): A

  def report_for_snapshot(lint_results: Linter.Lint_Report): A
}

object XML_Lint_Reporter extends Reporter[XML.Body] {

  def report_for_command(lint_report: Linter.Lint_Report, id: Document_ID.Command): XML.Body = {
    val xml = report_lints(lint_report.command_lints(id))
    if (xml.isEmpty) Nil
    else XML.elem(Markup.KEYWORD1, text("lints:")) :: xml
  }

  def report_for_snapshot(lint_report: Linter.Lint_Report): XML.Body =
    report_lints(
      lint_report.results,
      print_number = false,
      print_location = true,
      print_name = true
    )

  private def report_lints(
      lint_results: List[Linter.Lint_Result],
      print_number: Boolean = true,
      print_location: Boolean = false,
      print_name: Boolean = false
  ): XML.Body =
    lint_results.zipWithIndex
      .map(ri =>
        report_lint(
          ri._1,
          ri._2,
          print_number = print_number,
          print_location = print_location,
          print_name = print_name
        )
      )
      .flatten

  private def report_lint(
      lint_result: Linter.Lint_Result,
      lint_number: Int = 0,
      print_number: Boolean = true,
      print_location: Boolean = false,
      print_name: Boolean = false
  ): XML.Body = {

    val source = lint_result.commands.head.snapshot.node.source

    def text_range_to_line(range: Text.Range): Line.Range = Line.Document(source).range(range)

    val location = when(
      print_location,
      text(s"At ${text_range_to_line(lint_result.range).start.print}:\n")
    )

    val message =
      if (print_number)
        text(s" ${lint_number + 1}. ${lint_result.message}")
      else
        text(s" ${lint_result.message}")

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
