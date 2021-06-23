package linter

import isabelle._

class Linter_Interface {

  var lint_cache: Map[Document.Node.Name, (Document.Version, Linter.Lint_Report)] = Map.empty

  private def update_cache(snapshot: Document.Snapshot): Unit = {
    lazy val new_cache =
      lint_cache + (snapshot.node_name -> (snapshot.version, Linter.lint(
        snapshot,
        Lints.all_lints
      )))
    lint_cache get snapshot.node_name match {
      case None               => lint_cache = new_cache
      case Some((version, _)) => if (snapshot.version.id < version.id) lint_cache = new_cache
    }
  }

  def lint_results(snapshot: Document.Snapshot): List[Linter.Lint_Result] = {
    update_cache(snapshot)
    lint_cache(snapshot.node_name)._2.results.sortWith((r1, r2) =>
      Text.Range.Ordering.compare(r1.range, r2.range) < 0
    )
  }

  def lint_ranges(
      snapshot: Document.Snapshot,
      line_range: Text.Range = Text.Range.full
  ): List[Text.Info[Linter.Severity.Level]] =
    lint_results(snapshot)
      .filter(lint_result => !line_range.apart(lint_result.range))
      .map(lint_result => Text.Info(lint_result.range.restrict(line_range), lint_result.severity))
      .sortBy(_.info.id)

  def command_lints(
      snapshot: Document.Snapshot,
      command_id: Document_ID.Command
  ): List[Linter.Lint_Result] =
    lint_results(snapshot).filter(_.command.command.id == command_id).sortBy(_.severity.id)

}

class Linter_Variable {
  private val no_linter: Option[Linter_Interface] = None
  private var current_linter: Option[Linter_Interface] = no_linter

  def get: Option[Linter_Interface] = synchronized { current_linter }

  def update(options: Options): Unit = synchronized {
    if (options.bool("linter")) {
      if (current_linter.isEmpty) {
        current_linter = Some(new Linter_Interface)
      }
    } else current_linter = no_linter
  }
}
